"""Framed credential-challenging TLS fixture; importing has no side effects."""
import base64
import importlib.util
from pathlib import Path

_spec = importlib.util.spec_from_file_location(
    "credential_smart_http", Path(__file__).with_name("smart-http-fixture.py"))
_fixture = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(_fixture)


class _AuthRoutes(dict):
    def __init__(self, owner, routes):
        super().__init__(routes)
        self.owner = owner

    def denied(self):
        return self.owner.requests[-1][2].get("Authorization") != "Basic " + base64.b64encode(self.owner.credentials.encode()).decode()

    def __contains__(self, key):
        return self.denied() or super().__contains__(key)

    def __getitem__(self, key):
        if self.denied():
            return 401, {"WWW-Authenticate": 'Basic realm="fixture"'}, b""
        return super().__getitem__(key)


class CredentialHTTPServer(_fixture.SmartHTTPServer):
    """SmartHTTPServer API with properly framed TLS Basic challenges.

    `credentials` is fake USER:PASSWORD text.  `routes` apply after successful
    authentication.  `requests` retains every attempt for replay assertions.
    """
    def __init__(self, root, credentials="alice:password", **kwargs):
        if "auth" in kwargs:
            raise ValueError("use credentials, not auth")
        super().__init__(root, **kwargs)
        self.credentials = credentials
        self.routes = _AuthRoutes(self, self.routes)
