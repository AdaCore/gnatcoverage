#! /usr/bin/env python

"""
Decoder for source trace files.

This module can be used as a script to read and decode a source trace file,
possibly enabling debug output to investigate malformed files.
"""

from __future__ import absolute_import, division, print_function

import argparse
from contextlib import contextmanager
import struct


class ByteStreamDecoder(object):
    """
    Debug helper to analyze the process of decoding a binary stream.
    """

    BYTES_PER_LINE = 16

    def __init__(self, stream, enabled):
        """
        :param file stream: Byte stream to read.
        :param bool enable: If true, enable decoding traces
        """
        self.stream = stream
        self.enabled = enabled
        self.label_stack = []
        self.offset = 0

    def _print(self, message, *args, **kwargs):
        if not self.enabled:
            return
        if args or kwargs:
            message = message.format(*args, **kwargs)
        print('{}{}'.format('  ' * len(self.label_stack), message))

    @contextmanager
    def label_context(self, label):
        """
        Context manager to put a label on everything that is read while this
        context manager is active.

        :param str label: Label to use.
        """
        self._print('{} ({:#0x}):', label, self.offset)
        self.label_stack.append(label)
        yield
        self.label_stack.pop()

    def read(self, size):
        """
        Read bytes from this file.

        :param int size: Number of bytes to read.
        :rtype: str
        """
        bytes = self.stream.read(size)
        if self.enabled:

            # Two characters per byte and one space in between
            bytes_part_size = 3 * self.BYTES_PER_LINE - 1

            # Number of lines to represent bytes
            lines_count = (len(bytes) + self.BYTES_PER_LINE
                           - 1) // self.BYTES_PER_LINE

            for line in range(lines_count):
                start_byte = line * self.BYTES_PER_LINE
                end_byte = (line + 1) * self.BYTES_PER_LINE
                bytes_slice = bytes[start_byte:end_byte]

                bytes_part = ' '.join(
                    '{:02x}'.format(ord(b)) for b in bytes_slice
                ).ljust(bytes_part_size)

                ascii_part = ''.join(
                    '.' if b < ' ' or b > '~' else b
                    for b in bytes_slice
                )

                self._print('{} | {}'.format(bytes_part, ascii_part))
        self.offset += len(bytes)
        return bytes


class Struct(object):
    """
    Wrapper for struct.Struct to work with ByteStreamDecoder.
    """

    def __init__(self, label, *fields):
        """
        :param str label: Name of this struct.
        :param fields: List of couples (label, format) for the fields of this
            structure.
        :type fields: list[(str, str)]
        """
        self.label = label
        self.fields = [(name, struct.Struct(fmt)) for name, fmt in fields]

    def read(self, fp):
        """
        Read bytes from ``fp`` and decode these bytes according to the format
        of this structure.

        :param ByteStreamDecoder fp: Stream from which to read and decode this
            structure.
        """
        with fp.label_context(self.label):
            result = {}
            for i, (name, struct) in enumerate(self.fields):
                with fp.label_context(name):
                    buf = fp.read(struct.size)
                    assert (not buf and i == 0) or len(buf) == struct.size
                    if not buf:
                        return None
                    field = struct.unpack(buf)
                    if len(field) == 1:
                        field = field[0]
                    result[name] = field
            return result


trace_file_header_struct = Struct(
    'trace file header',
    ('magic', '32s'),
    ('format_version', 'I'),
    ('alignment', 'B'),
    ('endianity', 'B'),
    ('padding', 'H'),
)

trace_entry_header_struct = Struct(
    'trace entry header',
    ('closure_hash', 'I'),
    ('unit_name_length', 'I'),
    ('stmt_bit_count', 'I'),
    ('dc_bit_count', 'I'),
    ('mcdc_bit_count', 'I'),
    ('unit_kind', 'B'),
    ('bit_buffer_encoding', 'B'),
    ('padding', '2B'),
)


def read_aligned(fp, count, alignment):
    """
    Read the given number of bytes from the given stream, plus the required
    padding according to the given alignment. Return the bytes that were read,
    without the padding.
    """
    content = fp.read(count)
    if not content:
        return None
    assert len(content) == count
    padding_count = alignment - count % alignment
    if padding_count != alignment:
        padding = fp.read(padding_count)
        assert (len(padding) == padding_count and
                all(c == '\x00' for c in padding))
    return content


class SrcTraceFile(object):
    """
    In-memory representation of a source trace file.
    """

    def __init__(self, alignment, endianity, entries):
        self.alignment = alignment
        self.endianity = endianity
        self.entries = entries

    @classmethod
    def read(cls, fp):
        """
        Read a trace file from the `fp` file. Return a TraceFile instance.
        """
        header = trace_file_header_struct.read(fp)

        magic = header['magic']
        if magic != 'GNATcov source trace file' + '\x00' * 7:
            raise ValueError('Invalid magic: {}'.format(magic))

        format_version = header['format_version']
        if format_version != 0:
            raise ValueError('Unsupported format version: {}'
                             .format(format_version))

        alignment = header['alignment']
        if alignment not in (1, 2, 4, 8):
            raise ValueError('Invalid alignment: {}'.format(alignment))

        endianity = header['endianity']
        endianity = {0: 'little-endian', 1: 'big-endian'}[endianity]

        entries = []
        result = cls(alignment, endianity, entries)

        while True:
            entry = TraceEntry.read(fp, result)
            if not entry:
                break
            entries.append(entry)

        return result

    def dump(self):
        def format_buffer(b):
            bounds = ('[{}-{}]'.format(0, len(b.bits) - 1)
                      if len(b.bits) else '[empty range]')
            content = (' '.join(str(i) for i, bit in enumerate(b.bits) if bit)
                       or '<empty>')
            return '{} {}'.format(bounds, content)

        print('Source trace file:')
        print('  Alignment:', self.alignment)
        print('  Endianity:', self.endianity)
        print('')
        for e in self.entries:
            print('  Unit {} ({}, hash={:#0x})'.format(
                e.unit_name, e.unit_kind, e.closure_hash))
            print('  Stmt buffer: {}'.format(format_buffer(e.stmt_buffer)))
            print('  Dc buffer:   {}'.format(format_buffer(e.dc_buffer)))
            print('  MCDC buffer: {}'.format(format_buffer(e.mcdc_buffer)))
            print('')


class TraceEntry(object):
    """
    In-memory representation of a trace entry.
    """

    def __init__(self, unit_kind, unit_name, closure_hash, stmt_buffer,
                 dc_buffer, mcdc_buffer):
        self.unit_kind = unit_kind
        self.unit_name = unit_name
        self.closure_hash = closure_hash
        self.stmt_buffer = stmt_buffer
        self.dc_buffer = dc_buffer
        self.mcdc_buffer = mcdc_buffer

    @classmethod
    def read(cls, fp, trace_file):
        """
        Read a trace entry from the `fp` file. Return a TraceFile instance.
        """
        with fp.label_context('trace entry'):
            header = trace_entry_header_struct.read(fp)
            if not header:
                return None

            unit_kind = {
                0: 'body',
                1: 'spec',
                2: 'separate'
            }[header['unit_kind']]

            if header['padding'] != (0, ) * 2:
                raise ValueError('Invalid padding: {}'
                                 .format(header['padding']))

            bit_buffer_encoding = {
               0: 'lsb_first_bytes'
            }[header['bit_buffer_encoding']]

            with fp.label_context('unit name'):
                unit_name = read_aligned(
                    fp, header['unit_name_length'], trace_file.alignment)
            with fp.label_context('stmt buffer'):
                stmt_buffer = TraceBuffer.read(
                    fp, trace_file, bit_buffer_encoding,
                    header['stmt_bit_count'])
            with fp.label_context('dc buffer'):
                dc_buffer = TraceBuffer.read(
                    fp, trace_file, bit_buffer_encoding,
                    header['dc_bit_count'])
            with fp.label_context('mcdc buffer'):
                mcdc_buffer = TraceBuffer.read(
                    fp, trace_file, bit_buffer_encoding,
                    header['mcdc_bit_count'])

        return cls(unit_kind, unit_name, header['closure_hash'], stmt_buffer,
                   dc_buffer, mcdc_buffer)


class TraceBuffer(object):
    """
    In-memory representation of a coverage buffer.
    """

    def __init__(self, bits):
        self.bits = bits

    @classmethod
    def read(cls, fp, trace_file, bit_buffer_encoding, bit_count):
        assert bit_buffer_encoding == 'lsb_first_bytes'

        bytes_count = bit_count // 8
        if bit_count % 8:
            bytes_count += 1

        bytes_and_padding = read_aligned(fp, bytes_count, trace_file.alignment)

        bits = []
        for byte_index in range(bytes_count):
            byte = ord(bytes_and_padding[byte_index])
            for bit_index in range(8):
                global_bit_index = 8 * byte_index + bit_index
                if global_bit_index >= bit_count:
                    return cls(bits)

                bits.append(bool(byte & 1))
                byte = byte >> 1

        return cls(bits)


parser = argparse.ArgumentParser('Decode a source trace file')
parser.add_argument('--debug', '-d', action='store_true',
                    help='Enable debug traces')
parser.add_argument('trace-file', help='Source trace file to decode')


if __name__ == '__main__':
    args = parser.parse_args()
    with open(getattr(args, 'trace-file'), 'rb') as f:
        tf = SrcTraceFile.read(ByteStreamDecoder(f, args.debug))

    tf.dump()
