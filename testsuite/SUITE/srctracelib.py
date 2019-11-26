#! /usr/bin/env python

"""
Decoder for source trace files.

This module can be used as a script to read and decode a source trace file,
possibly enabling debug output to investigate malformed files.
"""

from __future__ import absolute_import, division, print_function

import argparse

from SUITE.stream_decoder import ByteStreamDecoder, Struct


trace_file_header_struct = Struct(
    'trace file header',
    ('magic', '32s'),
    ('format_version', 'I'),
    ('alignment', 'B'),
    ('endianity', 'B'),
    ('padding', 'H'),
)

trace_info_header_struct = Struct(
    'trace info header',
    ('kind', 'I'),
    ('length', 'I'),
)

trace_entry_header_struct = Struct(
    'trace entry header',
    ('unit_name_length', 'I'),
    ('stmt_bit_count', 'I'),
    ('dc_bit_count', 'I'),
    ('mcdc_bit_count', 'I'),
    ('unit_part', 'B'),
    ('bit_buffer_encoding', 'B'),
    ('fingerprint', '20B'),
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

    def __init__(self, alignment, endianity, info_entries, entries):
        self.alignment = alignment
        self.endianity = endianity
        self.info_entries = info_entries
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

        info_entries = []
        entries = []
        result = cls(alignment, endianity, info_entries, entries)

        while True:
            entry = TraceInfoEntry.read(fp, result)
            if entry.kind == 'end':
                break
            info_entries.append(entry)

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
            print('  Unit {} ({}, fingerprint={})'.format(
                e.unit_name, e.unit_part,
                ''.join('{:02x}'.format(b) for b in e.fingerprint)))
            print('  Stmt buffer: {}'.format(format_buffer(e.stmt_buffer)))
            print('  Dc buffer:   {}'.format(format_buffer(e.dc_buffer)))
            print('  MCDC buffer: {}'.format(format_buffer(e.mcdc_buffer)))
            print('')


class TraceInfoEntry(object):
    """
    In-memory representation of a trace info entry.
    """

    def __init__(self, kind, data):
        self.kind = kind
        self.data = data

    @classmethod
    def read(cls, fp, trace_file):
        """
        Read a trace info entry from the `fp` file. Return a TraceInfoEntry
        instance.
        """
        with fp.label_context('trace info'):
            header = trace_info_header_struct.read(fp)
            if not header:
                return None

            kind = {
                0: 'end',
                1: 'program-name',
                2: 'exec-date',
                3: 'user-data',
            }[header['kind']]

            if kind == 'end' and header['length']:
                raise ValueError('invalid "end" marker')

            with fp.label_context('data'):
                data = read_aligned(fp, header['length'], trace_file.alignment)

        return cls(kind, data)


class TraceEntry(object):
    """
    In-memory representation of a trace entry.
    """

    def __init__(self, unit_part, unit_name, fingerprint, stmt_buffer,
                 dc_buffer, mcdc_buffer):
        self.unit_part = unit_part
        self.unit_name = unit_name
        self.fingerprint = fingerprint
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

            unit_part = {
                0: 'body',
                1: 'spec',
                2: 'separate'
            }[header['unit_part']]

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

        return cls(unit_part, unit_name, header['fingerprint'], stmt_buffer,
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
