#!/usr/bin/env python3
"""Build backward deltas with more spans than one Linux iovec batch."""
import hashlib
import pathlib
import sys
import zlib


def varint(value):
    data = bytearray()
    while value >= 128:
        data.append((value & 127) | 128)
        value >>= 7
    return bytes(data + bytes([value]))


def padded_varint(value):
    data = bytearray(varint(value))
    data[-1] |= 128
    data.extend([128] * (8 - len(data)))
    data.append(0)
    return bytes(data)


def header(kind, size):
    data = bytearray([(kind << 4) | (size & 15)])
    size >>= 4
    while size:
        data[-1] |= 128
        data.append(size & 127)
        size >>= 7
    return bytes(data)


def offset(value):
    data = bytearray([value & 127])
    while value >> 7:
        value = (value >> 7) - 1
        data.insert(0, 128 | (value & 127))
    return bytes(data)


if len(sys.argv) > 2 and sys.argv[2] == 'branch':
    parents = [0, 0, 1, 1, 3, 3, 2, 2, 4, 6, 7]
    pack = b'PACK' + (2).to_bytes(4, 'big') + (12).to_bytes(4, 'big')
    data = [bytes(range(256)) * 8192]
    positions = [len(pack)]
    pack += header(3, len(data[0])) + zlib.compress(data[0])
    for index, parent in enumerate(parents, 1):
        base = data[parent]
        marker = bytes([65 + index, 65 + parent])
        target = base + marker
        delta = (varint(len(base)) + varint(len(target))
                 + bytes([0xf0]) + len(base).to_bytes(3, 'little')
                 + bytes([len(marker)]) + marker)
        positions.append(len(pack))
        pack += (header(6, len(delta)) + offset(len(pack) - positions[parent])
                 + zlib.compress(delta))
        data.append(target)
else:
    size = 1105
    pack = b'PACK' + (2).to_bytes(4, 'big') + (9).to_bytes(4, 'big')
    previous = len(pack)
    pack += header(3, size) + zlib.compress(b'a' * size)
    for index in range(8):
        start = len(pack)
        # Valid padded size headers exercise the full nine-byte decoder.
        encode = padded_varint if index == 0 else varint
        delta = encode(size) + encode(size) + bytes([1, 98 + index]) * size
        pack += header(6, len(delta)) + offset(start - previous) + zlib.compress(delta)
        previous = start
pathlib.Path(sys.argv[1]).write_bytes(pack + hashlib.sha1(pack).digest())
