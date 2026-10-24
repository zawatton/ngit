"""Receive a genuine push pack and return deliberately invalid report-status."""
import os
import subprocess
import sys

repo, mode = sys.argv[1:]
os.chdir(repo)
source = os.fdopen(0, "rb", buffering=0, closefd=False)
output = sys.stdout.buffer


def packet(data):
    output.write(f"{len(data) + 4:04x}".encode() + data)


old = subprocess.check_output(["git", "rev-parse", "refs/heads/main"]).strip()
packet(old + b" refs/heads/main\0report-status delete-refs\n")
output.write(b"0000")
output.flush()
updates = []
while True:
    header = source.read(4)
    if not header or header == b"0000":
        break
    data = source.read(int(header, 16) - 4).split(b"\0", 1)[0]
    updates.append(data.split()[2])
if updates:
    # The command parser uses unbuffered input so index-pack inherits the
    # exact pack boundary without losing bytes in a Python read-ahead buffer.
    subprocess.run(["git", "index-pack", "--stdin", "--fix-thin"],
                   stdin=source, stdout=subprocess.DEVNULL, check=True)
packet(b"unpack ok\n")
if mode == "partial":
    packet(b"ok " + updates[0] + b"\n")
elif mode == "duplicate":
    packet(b"ok " + updates[0] + b"\n")
    packet(b"ok " + updates[0] + b"\n")
elif mode == "unknown":
    packet(b"ok refs/heads/unknown\n")
elif mode == "malformed":
    packet(b"garbage\n")
elif mode in {"duplicate-complete", "unknown-complete"}:
    for ref in updates:
        packet(b"ok " + ref + b"\n")
    packet(b"ok " + (updates[0] if mode == "duplicate-complete"
                     else b"refs/heads/unknown") + b"\n")
output.write(b"0000")
output.flush()
