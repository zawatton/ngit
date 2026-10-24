#!/usr/bin/env python3
"""Write a self-contained pack with a REF delta followed by an OFS delta."""
import hashlib
import pathlib
import sys
import zlib


def varint(n):
    out = bytearray()
    while n >= 128:
        out.append((n & 127) | 128)
        n >>= 7
    return bytes(out + bytes([n]))


def header(kind, size):
    out = bytearray([(kind << 4) | (size & 15)])
    size >>= 4
    while size:
        out[-1] |= 128
        out.append(size & 127)
        size >>= 7
    return bytes(out)


def ofs(n):
    out = bytearray([n & 127])
    while n >> 7:
        n = (n >> 7) - 1
        out.insert(0, 128 | (n & 127))
    return bytes(out)


def delta(base, target):
    return varint(len(base)) + varint(len(target)) + bytes([len(target)]) + target


base, second, third = b'base payload\n', b'second payload\n', b'third payload\n'
oid = hashlib.sha1(b'blob ' + str(len(base)).encode() + b'\0' + base).digest()
pack = b'PACK' + (2).to_bytes(4, 'big') + (3).to_bytes(4, 'big')
pack += header(3, len(base)) + zlib.compress(base)
ref_offset = len(pack)
d = delta(base, second)
pack += header(7, len(d)) + oid + zlib.compress(d)
d = delta(second, third)
pack += header(6, len(d)) + ofs(len(pack) - ref_offset) + zlib.compress(d)
pathlib.Path(sys.argv[1]).write_bytes(pack + hashlib.sha1(pack).digest())
