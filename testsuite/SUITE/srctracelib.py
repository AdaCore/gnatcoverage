#! /usr/bin/env python

"""
Decoder for source trace files.

This module can be used as a script to read and decode a source trace file,
possibly enabling debug output to investigate malformed files.
"""

from __future__ import annotations

import argparse
from typing import IO

from SUITE.stream_decoder import ByteStreamDecoder, Struct, swap_bytes


trace_file_header_struct = Struct(
    "trace file header",
    ("magic", "32s"),
    ("format_version", "I"),
    ("alignment", "B"),
    ("endianity", "B"),
    ("padding", "H"),
)

trace_info_header_struct = Struct(
    "trace info header",
    ("kind", "I"),
    ("length", "I"),
)

trace_entry_header_struct = Struct(
    "trace entry header",
    ("unit_name_length", "I"),
    ("stmt_bit_count", "I"),
    ("dc_bit_count", "I"),
    ("mcdc_bit_count", "I"),
    ("language", "B"),
    ("unit_part", "B"),
    ("bit_buffer_encoding", "B"),
    ("fingerprint", "20s"),
    ("bit_maps_fingerprint", "20s"),
    ("annotations_fingerprint", "20s"),
    ("padding", "B"),
)


def read_aligned(
    fp: ByteStreamDecoder,
    count: int,
    alignment: int,
) -> bytes | None:
    """
    Read `count` bytes from the `fp` file, plus the required padding according
    to `alignment`. Return the bytes that were read, without the padding, or
    None if `count` is zero.
    """
    # If there was no payload bytes to read, there is no padding byte to read,
    # as the file offset, and thus the alignment, are preserved.
    if not count:
        return None

    content = fp.read(count)
    assert len(content) == count

    # Read padding bytes and ensure they are null
    padding_count = alignment - count % alignment
    if padding_count != alignment:
        padding = fp.read(padding_count)
        assert len(padding) == padding_count and all(c == 0 for c in padding)

    return content


def write_aligned(fp: IO[bytes], data: bytes, alignment: int) -> None:
    """
    Assuming that the number of bytes already written to the `fp` file is a
    multiple of `alignment`, write the given `data` to `fp`, plus the required
    padding bytes to maintain this alignment.
    """
    # If `data` is empty, there is no need for padding bytes as the alignment
    # is already correct.
    if not data:
        return

    fp.write(data)

    # Write padding bytes
    padding_count = alignment - len(data) % alignment
    if padding_count != alignment:
        padding = b"\x00" * padding_count
        fp.write(padding)


class SrcTraceFile:
    """
    In-memory representation of a source trace file.
    """

    MAGIC = b"GNATcov source trace file" + b"\x00" * 7

    FORMAT_VERSION = 4

    ENDIANITY_NAMES = {
        0: "little-endian",
        1: "big-endian",
    }
    ENDIANITY_CODES = {value: key for key, value in ENDIANITY_NAMES.items()}

    def __init__(
        self,
        alignment: int,
        endianity: str,
        info_entries: list[TraceInfoEntry],
        entries: list[TraceEntry],
    ):
        self.alignment = alignment
        self.endianity = endianity
        self.info_entries = info_entries
        self.entries = entries

        self.big_endian = endianity == "big-endian"

    @classmethod
    def read(cls, fp: ByteStreamDecoder) -> SrcTraceFile:
        """
        Read a trace file from the `fp` file. Return a TraceFile instance.
        """
        header = trace_file_header_struct.read(fp)
        assert header is not None

        magic = header["magic"]
        if magic != cls.MAGIC:
            raise ValueError(f"Invalid magic: {magic!r}")

        endianity_id = header["endianity"]
        assert isinstance(endianity_id, int)
        endianity = cls.ENDIANITY_NAMES[endianity_id]

        format_version = header["format_version"]
        assert isinstance(format_version, int)

        # We read the header as little-endian before knowing the trace file
        # endianity. This matters for the only multi-bytes field in this
        # header: the format version. Swap its bytes if the endianity is
        # actually big endian.
        if endianity == "big-endian":
            format_version = swap_bytes(format_version, 4)
        if format_version != cls.FORMAT_VERSION:
            raise ValueError(f"Unsupported format version: {format_version}")

        alignment = header["alignment"]
        assert isinstance(alignment, int)
        if alignment not in (1, 2, 4, 8, 16):
            raise ValueError(f"Invalid alignment: {alignment}")

        info_entries: list[TraceInfoEntry] = []
        entries: list[TraceEntry] = []
        result = cls(alignment, endianity, info_entries, entries)

        while True:
            info_entry = TraceInfoEntry.read(fp, result, result.big_endian)
            if info_entry is None or info_entry.kind == "end":
                break
            info_entries.append(info_entry)

        while True:
            entry = TraceEntry.read(fp, result, result.big_endian)
            if not entry:
                break
            entries.append(entry)

        return result

    def write(self, fp: IO[bytes]) -> None:
        """Write this source trace to the `fp` file."""
        big_endian = self.endianity == "big-endian"

        field_values: dict[str, int | bytes] = {
            "magic": self.MAGIC,
            "format_version": self.FORMAT_VERSION,
            "alignment": self.alignment,
            "endianity": self.ENDIANITY_CODES[self.endianity],
            "padding": 0,
        }
        trace_file_header_struct.write(fp, field_values, big_endian=big_endian)

        for info_entry in self.info_entries:
            info_entry.write(fp, big_endian, self.alignment)
        TraceInfoEntry("end", None).write(fp, big_endian, self.alignment)

        for entry in self.entries:
            entry.write(fp, big_endian, self.alignment)

    def dump(self) -> None:
        def format_buffer(b: TraceBuffer) -> str:
            bounds = (
                "[{}-{}]".format(0, len(b.bits) - 1)
                if len(b.bits)
                else "[empty range]"
            )
            content = (
                " ".join(str(i) for i, bit in enumerate(b.bits) if bit)
                or "<empty>"
            )
            return "{} {}".format(bounds, content)

        def format_fingerprint(fingerprint: bytes) -> str:
            return "".join("{:02x}".format(b) for b in fingerprint)

        print("Source trace file:")
        print("  Alignment: {}".format(self.alignment))
        print("  Endianity: {}".format(self.endianity))
        print("")
        for e in self.entries:
            print(
                f"  Unit {e.unit_name!r} ({e.unit_part},"
                f" SCOS hash={format_fingerprint(e.fingerprint)},"
                f" bit maps hash={format_fingerprint(e.bit_maps_fingerprint)},"
                " annotations hash="
                f"{format_fingerprint(e.annotations_fingerprint)})"
            )
            print("  Stmt buffer: {}".format(format_buffer(e.stmt_buffer)))
            print("  Dc buffer:   {}".format(format_buffer(e.dc_buffer)))
            print("  MCDC buffer: {}".format(format_buffer(e.mcdc_buffer)))
            print("")


class TraceInfoEntry:
    """
    In-memory representation of a trace info entry.
    """

    KIND_NAMES = {
        0: "end",
        1: "program-name",
        2: "exec-date",
        3: "user-data",
    }
    KIND_CODES = {value: key for key, value in KIND_NAMES.items()}

    def __init__(self, kind: str, data: bytes | None):
        self.kind = kind
        self.data = data

    @classmethod
    def read(
        cls,
        fp: ByteStreamDecoder,
        trace_file: SrcTraceFile,
        big_endian: bool,
    ) -> TraceInfoEntry | None:
        """
        Read a trace info entry from the `fp` file. Return a TraceInfoEntry
        instance.
        """
        with fp.label_context("trace info"):
            header = trace_info_header_struct.read(fp, big_endian=big_endian)
            if not header:
                return None

            kind_id = header["kind"]
            assert isinstance(kind_id, int)
            kind = cls.KIND_NAMES[kind_id]

            if kind == "end" and header["length"]:
                raise ValueError('invalid "end" marker')

            length = header["length"]
            assert isinstance(length, int)

            with fp.label_context("data"):
                data = read_aligned(fp, length, trace_file.alignment)

        return cls(kind, data)

    def write(self, fp: IO[bytes], big_endian: bool, alignment: int) -> None:
        """Write this trace info entry to the `fp` file."""
        field_values: dict[str, int | bytes] = {
            "kind": self.KIND_CODES[self.kind],
            "length": len(self.data) if self.data else 0,
        }
        trace_info_header_struct.write(fp, field_values, big_endian=big_endian)
        if self.data:
            write_aligned(fp, self.data, alignment)


class TraceEntry:
    """
    In-memory representation of a trace entry.
    """

    UNIT_PART_NAMES = {
        0: "not_applicable_part",
        1: "body",
        2: "spec",
        3: "separate",
    }
    UNIT_PART_CODES = {value: key for key, value in UNIT_PART_NAMES.items()}

    LANGUAGE_NAMES = {0: "unit_based", 1: "file_based"}
    LANGUAGE_CODES = {value: key for key, value in LANGUAGE_NAMES.items()}

    BIT_BUFFER_ENCODING_NAMES = {0: "lsb_first_bytes"}
    BIT_BUFFER_ENCODING_CODES = {
        value: key for key, value in BIT_BUFFER_ENCODING_NAMES.items()
    }

    def __init__(
        self,
        language: str,
        unit_part: str,
        unit_name: bytes,
        fingerprint: bytes,
        bit_maps_fingerprint: bytes,
        annotations_fingerprint: bytes,
        stmt_buffer: TraceBuffer,
        dc_buffer: TraceBuffer,
        mcdc_buffer: TraceBuffer,
    ):
        self.language = language
        self.unit_part = unit_part
        self.unit_name = unit_name
        self.fingerprint = fingerprint
        self.bit_maps_fingerprint = bit_maps_fingerprint
        self.annotations_fingerprint = annotations_fingerprint
        self.stmt_buffer = stmt_buffer
        self.dc_buffer = dc_buffer
        self.mcdc_buffer = mcdc_buffer

    @classmethod
    def read(
        cls,
        fp: ByteStreamDecoder,
        trace_file: SrcTraceFile,
        big_endian: bool,
    ) -> TraceEntry | None:
        """
        Read a trace entry from the `fp` file. Return a TraceFile instance.
        """
        with fp.label_context("trace entry"):
            header = trace_entry_header_struct.read(fp, big_endian=big_endian)
            if not header:
                return None

            unit_part_id = header["unit_part"]
            assert isinstance(unit_part_id, int)
            unit_part = cls.UNIT_PART_NAMES[unit_part_id]

            language_id = header["language"]
            assert isinstance(language_id, int)
            language = cls.LANGUAGE_NAMES[language_id]

            padding = header["padding"]
            assert isinstance(padding, int)
            if padding != 0:
                raise ValueError(f"Invalid padding: {padding}")

            bit_buffer_encoding_id = header["bit_buffer_encoding"]
            assert isinstance(bit_buffer_encoding_id, int)
            bit_buffer_encoding = cls.BIT_BUFFER_ENCODING_NAMES[
                bit_buffer_encoding_id
            ]

            unit_name_length = header["unit_name_length"]
            assert isinstance(unit_name_length, int)

            fingerprint = header["fingerprint"]
            assert isinstance(fingerprint, bytes)

            bit_maps_fingerprint = header["bit_maps_fingerprint"]
            assert isinstance(bit_maps_fingerprint, bytes)

            annotations_fingerprint = header["annotations_fingerprint"]
            assert isinstance(annotations_fingerprint, bytes)

            stmt_bit_count = header["stmt_bit_count"]
            assert isinstance(stmt_bit_count, int)

            dc_bit_count = header["dc_bit_count"]
            assert isinstance(dc_bit_count, int)

            mcdc_bit_count = header["mcdc_bit_count"]
            assert isinstance(mcdc_bit_count, int)

            with fp.label_context("unit name"):
                unit_name = read_aligned(
                    fp, unit_name_length, trace_file.alignment
                )
                assert isinstance(unit_name, bytes)

            with fp.label_context("stmt buffer"):
                stmt_buffer = TraceBuffer.read(
                    fp, trace_file, bit_buffer_encoding, stmt_bit_count
                )
            with fp.label_context("dc buffer"):
                dc_buffer = TraceBuffer.read(
                    fp, trace_file, bit_buffer_encoding, dc_bit_count
                )
            with fp.label_context("mcdc buffer"):
                mcdc_buffer = TraceBuffer.read(
                    fp, trace_file, bit_buffer_encoding, mcdc_bit_count
                )

        return cls(
            language,
            unit_part,
            unit_name,
            fingerprint,
            bit_maps_fingerprint,
            annotations_fingerprint,
            stmt_buffer,
            dc_buffer,
            mcdc_buffer,
        )

    def write(self, fp: IO[bytes], big_endian: bool, alignment: int) -> None:
        """Write this trace info entry to the `fp` file."""
        field_values: dict[str, int | bytes] = {
            "unit_name_length": len(self.unit_name),
            "stmt_bit_count": len(self.stmt_buffer.bits),
            "dc_bit_count": len(self.dc_buffer.bits),
            "mcdc_bit_count": len(self.mcdc_buffer.bits),
            "language": self.LANGUAGE_CODES[self.language],
            "unit_part": self.UNIT_PART_CODES[self.unit_part],
            "bit_buffer_encoding": self.BIT_BUFFER_ENCODING_CODES[
                "lsb_first_bytes"
            ],
            "fingerprint": self.fingerprint,
            "bit_maps_fingerprint": self.bit_maps_fingerprint,
            "annotations_fingerprint": self.annotations_fingerprint,
            "padding": 0,
        }
        trace_entry_header_struct.write(
            fp, field_values, big_endian=big_endian
        )

        write_aligned(fp, self.unit_name, alignment)

        self.stmt_buffer.write(fp, alignment)
        self.dc_buffer.write(fp, alignment)
        self.mcdc_buffer.write(fp, alignment)


class TraceBuffer:
    """
    In-memory representation of a coverage buffer.
    """

    def __init__(self, bits: list[bool]):
        self.bits = bits

    @staticmethod
    def byte_count(bit_count: int) -> int:
        """
        Return the number of bytes required to represent the given number of
        bits.
        """
        bytes_count = bit_count // 8
        if bit_count % 8:
            bytes_count += 1
        return bytes_count

    @classmethod
    def read(
        cls,
        fp: ByteStreamDecoder,
        trace_file: SrcTraceFile,
        bit_buffer_encoding: str,
        bit_count: int,
    ) -> TraceBuffer:
        assert bit_buffer_encoding == "lsb_first_bytes"

        bytes_count = cls.byte_count(bit_count)
        bytes_and_padding = read_aligned(fp, bytes_count, trace_file.alignment)

        bits: list[bool] = []
        if bytes_count > 0:
            assert isinstance(bytes_and_padding, bytes)
            for byte_index in range(bytes_count):
                byte = bytes_and_padding[byte_index]
                for bit_index in range(8):
                    global_bit_index = 8 * byte_index + bit_index
                    if global_bit_index >= bit_count:
                        return cls(bits)

                    bits.append(bool(byte & 1))
                    byte = byte >> 1

        return cls(bits)

    def write(self, fp: IO[bytes], alignment: int) -> None:
        """Write this coverage buffer to the `fp` file."""
        bit_count = len(self.bits)
        bytes_count = self.byte_count(bit_count)
        buffer = [0] * bytes_count

        for global_bit_index, bit in enumerate(self.bits):
            byte_index = global_bit_index // 8
            bit_index = global_bit_index % 8
            bit_value = int(bit) << bit_index
            buffer[byte_index] |= bit_value

        write_aligned(fp, bytes(byte for byte in buffer), alignment)


parser = argparse.ArgumentParser("Decode a source trace file")
parser.add_argument(
    "--debug", "-d", action="store_true", help="Enable debug traces"
)
parser.add_argument("trace-file", help="Source trace file to decode")


if __name__ == "__main__":
    args = parser.parse_args()
    with open(getattr(args, "trace-file"), "rb") as f:
        tf = SrcTraceFile.read(ByteStreamDecoder(f, args.debug))

    tf.dump()
