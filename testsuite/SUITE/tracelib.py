"""
Qemu trace files loading and saving library.
"""

from __future__ import annotations

import argparse
import binascii
from collections.abc import Iterator
import dataclasses
import datetime
import enum
import os
from typing import IO, TYPE_CHECKING

from SUITE.stream_decoder import ByteStreamDecoder, Struct


class TraceKind(enum.IntEnum):
    """
    Kind of trace file. See Trace_Kind in qemu_traces.ads.
    """

    Flat = 0
    History = 1
    Info = 2
    DecisionMap = 3


class InfoKind(enum.IntEnum):
    """
    Kind of trace information record. See Info_Kind_Type in qemu_traces.ads.
    """

    InfoEnd = 0
    ExecFileName = 1
    CoverageOptions = 2
    UserData = 3
    DateTime = 4
    Kernel_File_Name = 5
    ExecFileSize = 6
    ExecTimeStamp = 7
    ExecFileCRC32 = 8
    CoverageContext = 9
    ExecCodeSize = 10


class TraceOp(enum.IntEnum):
    """
    Trace operation bitmasks. See Trace_Op_* in qemu_traces.ads.
    """

    Block = 0x10
    Fault = 0x20
    Br0 = 0x01
    Br1 = 0x02
    Special = 0x80

    # Helper for the format_flags method, below
    flag_chars = enum.nonmember(
        [
            (Block, "B"),
            (Fault, "F"),
            (Br0, "b"),
            (Br1, "f"),
            (Special, "s"),
        ]
    )

    @classmethod
    def format_flags(cls, flags: int) -> str:
        """
        Return a human-readable representation for the given TraceOp value.

        This returns a compact representation: "-----" for no bit set, "B-bf-"
        for the block, br0 and br1 bits set, etc. This compact representation
        has always the same length, and is thus suitable for tabular printings.

        :param int flags: Integer value for the TraceOp to decode. This is the
            integer that is read directly from the trace file.
        """
        return "".join(
            char if flags & v else "-" for v, char in cls.flag_chars
        )


class TraceSpecial(enum.IntEnum):
    """
    Special trace operations. See Trace_Special_* in qemu_traces.ads.
    """

    Loadaddr = 1
    LoadSharedObject = 2
    UnloadSharedObject = 3


TRACE_MAGIC = b"#QEMU-Traces"
"""
Exepected value of the Magic header field. See Qemu_Trace_Magic in
qemu_traces.ads.
"""


if TYPE_CHECKING:
    HeaderTuple = tuple[bytes, int, int, int, bool, int, int, int]


@dataclasses.dataclass(frozen=True)
class TraceHeader:
    """
    See Trace_Header in qemu_traces.ads.
    """

    kind: int
    """
    Trace file kind. See TraceKind.
    """

    pc_size: int
    """
    Size of Program Counter on target, in bytes.
    """

    big_endian: bool
    """
    Whether the target is big-endian.
    """

    machine: int
    """
    Target ELF machine ID.
    """

    @property
    def bits(self) -> int:
        """
        Return size of PC on target, in bits.
        """
        assert self.pc_size in (4, 8)
        return self.pc_size * 8

    @property
    def struct_tuple(self) -> HeaderTuple:
        return (
            TRACE_MAGIC,
            1,
            self.kind,
            self.pc_size,
            self.big_endian,
            self.machine >> 8,
            self.machine & 0xFF,
            0,
        )

    @classmethod
    def read(cls, fp: ByteStreamDecoder) -> TraceHeader | None:
        """
        Read a trace file header from the `fp` file and return it.
        """
        fields = unpack_from_file(fp, TraceHeaderStruct)
        if fields is None:
            return None

        kind = fields[2]
        assert isinstance(kind, int)

        pc_size = fields[3]
        assert isinstance(pc_size, int) and pc_size in (4, 8)

        big_endian = fields[4]
        assert isinstance(big_endian, int)

        machine_hi = fields[5]
        machine_lo = fields[6]
        assert isinstance(machine_hi, int)
        assert isinstance(machine_lo, int)

        return cls(
            kind, pc_size, bool(big_endian), machine_hi << 8 | machine_lo
        )

    def write(self, fp: IO[bytes]) -> None:
        """
        Write this trace file header to the `fp` file.
        """
        TraceHeaderStruct.write(fp, self.struct_tuple)


TraceHeaderStruct = Struct(
    "trace file header",
    # See Trace_Header in qemu_traces.ads
    ("magic", "12s"),
    ("version", "B"),
    ("kind", "B"),
    ("sizeof_target_pc", "B"),
    ("big_endian", "B"),
    ("machine_hi", "B"),
    ("machine_lo", "B"),
    ("padding", "H"),
)


TraceInfoHeaderStruct = Struct(
    "trace info header",
    # See Trace_Info_Header in qemu_traces.ads
    ("kind", "I"),
    ("length", "I"),
)


TraceEntry32Struct = Struct(
    "trace entry 32",
    # See Trace_Entry32 in qemu_traces.ads
    ("pc", "I"),
    ("size", "H"),
    ("op", "B"),
    ("padding", "B"),
)
TraceEntry64Struct = Struct(
    "trace entry 64",
    # See Trace_Entry64 in qemu_traces.ads
    ("pc", "Q"),
    ("size", "H"),
    ("op", "B"),
    ("padding0", "B"),
    ("padding1", "I"),
)


def unpack_from_file(
    fp: ByteStreamDecoder,
    struct: Struct,
) -> list[tuple[int | bytes, ...] | int | bytes] | None:
    """
    Read all the bytes necessary to decode `struct` from the `fp` file and
    return the decoded structure as a tuple.

    :param fp: File from which to read bytes.
    :param struct: Struct instance to decode data.
    """
    fields = struct.read(fp)
    if fields is None:
        return None
    return [fields[name] for name, _ in struct.be_fields]


class TraceFile:
    """
    In-memory representation of a trace file.
    """

    def __init__(
        self,
        first_header: TraceHeader,
        infos: TraceInfoList,
        second_header: TraceHeader | None,
        entries: list[TraceEntry],
    ):
        """
        Create a trace file.

        If this instance must represent a partial trace file, `second_header`
        must be None and `entries` must be empty.

        :param first_header: First header.
        :param infos: Information for the traced program.
        :param second_header: Second header (if present).
        :param entries: Trace entries.
        """
        self.first_header = first_header
        assert self.first_header

        self.bits = first_header.bits
        self.infos = infos

        self.second_header = second_header

        # If there is no second header, make sure we have no entry
        if not second_header:
            assert not entries
            return

        self.entries = entries
        for entry in entries:
            assert entry.bits == self.bits

    @classmethod
    def read(cls, fp: ByteStreamDecoder) -> TraceFile:
        """
        Read a trace file from the `fp` file. Return a TraceFile instance.
        """
        first_header = TraceHeader.read(fp)
        assert first_header is not None
        bits = first_header.bits
        infos = TraceInfoList.read(fp)

        second_header = TraceHeader.read(fp)
        entries = []
        if second_header:
            while True:
                entry = TraceEntry.read(fp, bits)
                if not entry:
                    break
                entries.append(entry)

        return cls(first_header, infos, second_header, entries)

    def write(self, fp: IO[bytes]) -> None:
        """
        Write this trace file to the `fp` file.
        """
        assert self.bits

        self.first_header.write(fp)
        self.infos.write(fp)
        if self.second_header:
            self.second_header.write(fp)
            for entry in self.entries:
                entry.write(fp)

    def iter_entries(self, raw: bool = False) -> Iterator[TraceEntry]:
        """
        Yield all trace entries in this trace file.

        Unless `raw` is true, this interprets special trace entries, such as
        loadaddr (module loaded at PC). In this case, other trace entries are
        returned (and potentially skipped) accordingly.
        """
        entries = iter(self.entries)
        offset = 0

        # If there is a kernel, skip all trace entries until we get a loadaddr
        # special one.
        if not raw and InfoKind.Kernel_File_Name in self.infos.infos:
            while True:
                loadaddr = next(entries)
                if (
                    loadaddr.is_special
                    and loadaddr.size == TraceSpecial.Loadaddr
                ):
                    offset = loadaddr.pc
                    break

        # Now go through the remaining list of trace entries
        for e in entries:
            # TODO: handle special trace entries that can show up here
            # (load_shared_object, ...)
            assert not e.is_special

            # Discard trace entries for code below the module of interest
            if not raw and e.pc < offset:
                continue

            yield TraceEntry(e.bits, e.pc - offset, e.size, e.op, e.infos)


class TraceInfo:
    """
    In-memory representation for a trace info entry.
    """

    def __init__(self, kind: InfoKind, data: bytes):
        """
        Create a trace info entry.

        :param kind: Trace info entry kind.
        :param data: Raw data (bytes) for this entry.
        """
        self.kind = kind
        self.data = data

    @staticmethod
    def padding_size(data_size: int) -> int:
        """
        Number of padding bytes at the end of the Trace_Info record.
        """
        return 0 if data_size % 4 == 0 else (4 - (data_size % 4))

    @property
    def is_end(self) -> bool:
        """
        Whether this trace info entry is the trace info list end marker.
        """
        return self.kind == InfoKind.InfoEnd

    @classmethod
    def read(cls, fp: ByteStreamDecoder) -> TraceInfo:
        """
        Read a trace info entry from the `fp` file. Return a TraceInfo
        instance.
        """
        with fp.label_context("trace info"):
            hdr = unpack_from_file(fp, TraceInfoHeaderStruct)
            assert hdr
            kind, length = hdr
            assert isinstance(kind, int)
            assert isinstance(length, int)

            with fp.label_context("data"):
                data = fp.read(length)
                assert len(data) == length

            with fp.label_context("padding"):
                padding_size = cls.padding_size(len(data))
                padding = fp.read(padding_size)
                assert (
                    len(padding) == padding_size
                ), "Expected {} padding bytes but got {}".format(
                    padding_size, len(padding)
                )
                assert padding == (
                    b"\x00" * padding_size
                ), "Some padding bytes are non-null: {}".format(repr(padding))

        return cls(InfoKind(kind), data)

    def write(self, fp: IO[bytes]) -> None:
        """
        Write this trace info to the `fp` file.
        """
        TraceInfoHeaderStruct.write(fp, (self.kind, len(self.data)))
        fp.write(self.data)
        fp.write(b"\x00" * self.padding_size(len(self.data)))


class TraceInfoList:
    """
    In-memory representation of a list of list of trace info entries.
    """

    def __init__(self, infos: dict[int, TraceInfo] | None = None):
        """
        Create a list of trace info entries.

        :param infos: None for an empty list.  Otherwise, mapping from trace
            info kind (see InfoKind) to TraceInfo instance.
        """
        self.infos = infos or {}

    @classmethod
    def read(cls, fp: ByteStreamDecoder) -> TraceInfoList:
        """
        Read a trace info entry list from the `fp` file. Return a TraceInfoList
        instance.
        """
        result = cls()
        while True:
            info = TraceInfo.read(fp)
            if info.is_end:
                break
            result.infos[info.kind] = info
        return result

    def write(self, fp: IO[bytes]) -> None:
        """
        Write this trace info list to the `fp` file.
        """
        for _, info in sorted(self.infos.items()):
            info.write(fp)
        TraceInfoHeaderStruct.write(fp, (InfoKind.InfoEnd, 0))


class TraceEntry:
    """
    In-memory representation of a trace entry.
    """

    def __init__(
        self,
        bits: int,
        pc: int,
        size: int,
        op: int,
        infos: TraceInfoList | None = None,
    ):
        """
        :param bits: Number of bits in the target PC.
        :param pc: PC for the trace entry (i.e. first address to be
            executed, or address of the event).
        :param size: Size in bytes for the trace entry.
        :param infos: Informations associated to this trace entry.
        """
        self.bits = bits
        self.pc = pc
        self.size = size
        self.op = op
        self.infos = infos

    @property
    def is_special(self) -> bool:
        """
        Whether this is a special trace entry. See Trace_Op_Special in
        qemu_traces.ads.
        """
        return bool(self.op & TraceOp.Special)

    @staticmethod
    def struct(bits: int) -> Struct:
        """
        Struct instance to decode a trace entry given the number of bits in PC
        PC on a target.
        """
        return {
            32: TraceEntry32Struct,
            64: TraceEntry64Struct,
        }[bits]

    @classmethod
    def read(cls, fp: ByteStreamDecoder, bits: int) -> TraceEntry | None:
        """
        Read a trace entry from the `fp` file. Return a TraceEntry instance.
        """
        with fp.label_context("trace entry"):
            fields = unpack_from_file(fp, cls.struct(bits))
            if not fields:
                return None
            pc, size, op = fields
            assert isinstance(pc, int)
            assert isinstance(size, int)
            assert isinstance(op, int)

            # Remove padding
            padding = fields.pop()
            assert padding == 0, repr(padding)
            if bits == 64:
                padding = fields.pop()
                assert padding == 0, repr(padding)

            result = cls(bits, pc, size, op)
            if (
                result.is_special
                and result.size == TraceSpecial.LoadSharedObject
            ):
                result.infos = TraceInfoList.read(fp)
            return result

    def write(self, fp: IO[bytes]) -> None:
        """
        Write this trace entry to the `fp` file.
        """
        fields = [self.pc, self.size, self.op, 0]
        if self.bits == 64:
            fields.append(0)
        self.struct(self.bits).write(fp, fields)
        if self.infos:
            self.infos.write(fp)


def create_exec_infos(
    filename: str,
    code_size: int | None = None,
) -> TraceInfoList:
    """
    Create a TraceInfoList object to describe the given executable.

    :param filename: Path to the executable file.
    :param code_size: If provided, size of the executable code section in
        `filename`. Used to create the Exec_Code_Size trace information entry.
    """
    stat = os.stat(filename)
    with open(filename, "rb") as f:
        f_contents = f.read()
    crc32 = binascii.crc32(f_contents) & 0xFFFFFFFF
    mtime = datetime.datetime.utcfromtimestamp(int(stat.st_mtime))

    def create_trace_info(kind: InfoKind, data_str: str) -> TraceInfo:
        return TraceInfo(kind, data_str.encode("utf-8"))

    infos = [
        create_trace_info(InfoKind.ExecFileName, filename),
        create_trace_info(InfoKind.ExecFileSize, " " + str(stat.st_size)),
        create_trace_info(InfoKind.ExecTimeStamp, mtime.isoformat(" ")),
        create_trace_info(InfoKind.ExecFileCRC32, " " + str(crc32)),
    ]
    if code_size:
        infos.append(
            create_trace_info(InfoKind.ExecCodeSize, " " + str(code_size))
        )

    return TraceInfoList({info.kind: info for info in infos})


parser = argparse.ArgumentParser("Decode a binary trace file")
parser.add_argument(
    "--debug", "-d", action="store_true", help="Enable debug traces"
)
parser.add_argument("trace-file", help="Binary trace file to decode")


if __name__ == "__main__":
    args = parser.parse_args()
    with open(getattr(args, "trace-file"), "rb") as f:
        tf = TraceFile.read(ByteStreamDecoder(f, args.debug, 4))

    # TODO: add trace-file dump capabilities
