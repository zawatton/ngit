"""Owned loopback smart HTTP(S) fixture; importing starts no processes."""
import base64
import contextlib
import http.server
import os
from pathlib import Path
import ssl
import subprocess
import sys
import tempfile
import threading
import urllib.parse


class SmartHTTPServer(contextlib.AbstractContextManager):
    def __init__(self, root, tls=True, auth=None, headers=None, routes=None,
                 chunked=False, hostname="localhost", before_rpc=None,
                 forward_protocol=False, rewrite_response=None,
                 backend_env=None, run_process=None):
        self.root = Path(root).resolve()
        self.tls, self.auth = tls, auth
        self.headers = headers or {}
        self.routes = routes or {}
        self.chunked, self.hostname = chunked, hostname
        self.requests = []
        self.errors = []
        self.before_rpc = before_rpc
        # Opt-in protocol support leaves existing v0 fixture users unchanged.
        self.forward_protocol = forward_protocol
        self.rewrite_response = rewrite_response
        self.backend_env = backend_env
        self.run_process = run_process or subprocess.run

    def __enter__(self):
        owner = self
        class Handler(http.server.BaseHTTPRequestHandler):
            def log_message(self, *args):
                pass

            def do_GET(self):
                self.respond()

            def do_POST(self):
                self.respond()

            def respond(self):
                if self.headers.get("Transfer-Encoding", "").lower() == "chunked":
                    chunks = []
                    while True:
                        size = int(self.rfile.readline().split(b";", 1)[0], 16)
                        if not size:
                            while self.rfile.readline() not in (b"\r\n", b"\n", b""):
                                pass
                            break
                        chunk = self.rfile.read(size)
                        if len(chunk) != size or self.rfile.read(2) != b"\r\n":
                            raise ValueError("truncated chunked request")
                        chunks.append(chunk)
                    body = b"".join(chunks)
                else:
                    body = self.rfile.read(int(self.headers.get("Content-Length", "0")))
                self.request_body = body
                owner.requests.append((self.command, self.path, dict(self.headers), body,
                                       list(self.headers.items())))
                if owner.auth and self.headers.get("Authorization") != "Basic " + base64.b64encode(owner.auth.encode()).decode():
                    self.send_response(401)
                    self.send_header("WWW-Authenticate", 'Basic realm="fixture"')
                    self.end_headers()
                    return
                if any(self.headers.get(k) != v for k, v in owner.headers.items()):
                    self.send_response(403)
                    self.end_headers()
                    return
                if self.path in owner.routes:
                    route = owner.routes[self.path]
                    code, headers, response = route(self) if callable(route) else route
                else:
                    if self.command == "POST" and owner.before_rpc:
                        callback, owner.before_rpc = owner.before_rpc, None
                        callback()
                    path = urllib.parse.urlsplit(self.path)
                    env = dict(os.environ if owner.backend_env is None else owner.backend_env,
                               GIT_PROJECT_ROOT=str(owner.root),
                               GIT_HTTP_EXPORT_ALL="1", PATH_INFO=urllib.parse.unquote(path.path),
                               QUERY_STRING=path.query, REQUEST_METHOD=self.command,
                               CONTENT_TYPE=self.headers.get("Content-Type", ""),
                               CONTENT_LENGTH=str(len(body)), REMOTE_USER="fixture",
                               GIT_PROTOCOL="version=0")
                    # remote-curl gzips larger negotiation requests. The CGI
                    # backend inflates them only when this header is forwarded;
                    # CONTENT_LENGTH still describes the compressed body above.
                    env.pop("HTTP_CONTENT_ENCODING", None)
                    if self.headers.get("Content-Encoding") is not None:
                        env["HTTP_CONTENT_ENCODING"] = self.headers["Content-Encoding"]
                    if owner.forward_protocol:
                        env.pop("GIT_PROTOCOL", None)
                        if self.headers.get("Git-Protocol") is not None:
                            env["GIT_PROTOCOL"] = self.headers["Git-Protocol"]
                    proc = owner.run_process(["git", "http-backend"], input=body, env=env,
                                          stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=20)
                    head, sep, response = proc.stdout.partition(b"\r\n\r\n")
                    if not sep:
                        raise RuntimeError(proc.stderr)
                    headers, code = {}, 200
                    for line in head.decode().split("\r\n"):
                        k, v = line.split(":", 1)
                        if k.lower() == "status":
                            code = int(v.strip().split()[0])
                        else:
                            headers[k] = v.strip()
                if owner.rewrite_response:
                    code, headers, response = owner.rewrite_response(self, code, headers, response)
                self.send_response(code)
                for k, v in headers.items():
                    self.send_header(k, v)
                if owner.chunked:
                    self.send_header("Transfer-Encoding", "chunked")
                else:
                    self.send_header("Content-Length", str(len(response)))
                self.end_headers()
                try:
                    if owner.chunked:
                        for offset in range(0, len(response), 37):
                            part = response[offset:offset+37]
                            self.wfile.write(f"{len(part):x}\r\n".encode() + part + b"\r\n")
                        self.wfile.write(b"0\r\n\r\n")
                    else:
                        self.wfile.write(response)
                except (BrokenPipeError, ConnectionResetError):
                    pass
        class Server(http.server.ThreadingHTTPServer):
            def handle_error(self, request, address):
                # TLS rejection can reset a socket before an HTTP request.
                error = sys.exc_info()[1]
                if not isinstance(error, (ConnectionResetError, BrokenPipeError, ssl.SSLError)):
                    owner.errors.append(repr(error))
        self.server = Server(("127.0.0.1", 0), Handler)
        self.temp = tempfile.TemporaryDirectory(prefix="ngit-http-cert-")
        self.ca_file = str(Path(self.temp.name) / "cert.pem")
        if self.tls:
            key = str(Path(self.temp.name) / "key.pem")
            self.run_process(["openssl", "req", "-x509", "-newkey", "rsa:2048", "-nodes",
                            "-keyout", key, "-out", self.ca_file, "-days", "1",
                            "-subj", "/CN=localhost", "-addext", "subjectAltName=DNS:localhost"],
                           check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                           env=self.backend_env, timeout=20)
            ctx = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
            ctx.load_cert_chain(self.ca_file, key)
            self.server.socket = ctx.wrap_socket(self.server.socket, server_side=True)
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()
        return self

    def url(self, name=""):
        return f"{'https' if self.tls else 'http'}://{self.hostname}:{self.server.server_port}/{name}"

    def __exit__(self, *args):
        self.server.shutdown()
        self.server.server_close()
        self.thread.join(timeout=5)
        self.temp.cleanup()
        if self.errors:
            raise RuntimeError("HTTP fixture errors: " + "; ".join(self.errors))
