"""
Qemu trace files loading and saving library.
"""


import binascii
import datetime
import os
from struct import Struct


class Enum(object):
    RANGE = None

    @classmethod
    def in_range(cls, value):
        low, high = cls.RANGE
        return low <= value <= high

    @classmethod
    def check(cls, value):
        assert cls.in_range(value), 'Invalid {}: {} not in {} .. {}'.format(
            cls.__name__,
            value,
            cls.RANGE[0], cls.RANGE[1]
        )


class TraceKind(Enum):
    Flat = 0
    History = 1
    Info = 2
    DecisionMap = 3

    RANGE = (0, 3)


class InfoKind(Enum):
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

    RANGE = (0, 10)


class TraceOp(Enum):
    Block   = 0x10
    Fault   = 0x20
    Br0     = 0x01
    Br1     = 0x02
    Special = 0x80


class TraceSpecial(Enum):
    Loadaddr = 1
    LoadSharedObject = 2
    UnloadSharedObject = 3

    RANGE = (1, 3)


TRACE_MAGIC = '#QEMU-Traces'
def create_trace_header(kind, pc_size, big_endian, machine):
    return (
        TRACE_MAGIC,
        1,
        kind,
        pc_size,
        big_endian,
        machine >> 8,
        machine & 0xff
    )

TraceHeaderStruct = Struct(
    '12s'  # Magic string
    'B'    # Version of file format
    'B'    # Section kind

    'B'    # Size of PC on target, in bytes
    'B'    # Whether the host is big endian

    'BB'   # Target ELF machine ID
    'xx'   # Padding
)


TraceInfoHeaderStruct = Struct(
    'I'  # Kind
    'I'  # Length
)


TraceEntry32Struct = Struct(
    'I'  # Pc
    'H'  # Size
    'B'  # Op
    'x'  # Padding
)
TraceEntry64Struct = Struct(
    'Q'      # Pc
    'H'      # Size
    'B'      # Op
    'xxxxx'  # Padding
)


def unpack_from_file(fp, struct):
    buf = fp.read(struct.size)
    if not buf:
        return None
    assert len(buf) == struct.size
    return struct.unpack(buf)


class TraceFile(object):
    def __init__(self, first_header, infos, second_header, entries):
        self.bits = self.bits(second_header)

        self.first_header = first_header
        assert self.first_header

        self.infos = infos

        self.second_header = second_header
        assert self.second_header

        self.entries = entries
        for entry in entries:
            assert entry.bits == self.bits

    @staticmethod
    def bits(header):
        assert header[3] in (4, 8)
        return header[3] * 8

    @classmethod
    def read(cls, fp):
        first_header = unpack_from_file(fp, TraceHeaderStruct)
        infos = TraceInfoList.read(fp)

        second_header = unpack_from_file(fp, TraceHeaderStruct)
        bits = cls.bits(first_header)

        entries = []
        while True:
            entry = TraceEntry.read(fp, bits)
            if not entry:
                break
            entries.append(entry)

        return cls(first_header, infos, second_header, entries)

    def write(self, fp):
        assert self.bits

        fp.write(TraceHeaderStruct.pack(*self.first_header))
        self.infos.write(fp)
        fp.write(TraceHeaderStruct.pack(*self.second_header))
        for entry in self.entries:
            entry.write(fp)


class TraceInfo(object):
    def __init__(self, kind, data):
        InfoKind.check(kind)
        self.kind = kind
        self.data = data

    @staticmethod
    def padding_size(data_size):
        return 0 if data_size % 4 == 0 else (4 - (data_size % 4))

    @property
    def is_end(self):
        return self.kind == InfoKind.InfoEnd

    @classmethod
    def read(cls, fp):
        hdr = unpack_from_file(fp, TraceInfoHeaderStruct)
        assert hdr
        kind, length = hdr
        InfoKind.check(kind)

        data = fp.read(length)
        assert len(data) == length

        padding_size = cls.padding_size(len(data))
        padding = fp.read(padding_size)
        assert len(padding) == padding_size, (
            'Expected {} padding bytes but got {}'.format(
                padding_size, len(padding)
            )
        )
        assert padding == ('\x00' * padding_size), (
            'Some padding bytes are non-null: {}'.format(repr(padding))
        )

        return cls(kind, data)

    def write(self, fp):
        fp.write(TraceInfoHeaderStruct.pack(self.kind, len(self.data)))
        fp.write(self.data)
        fp.write('\x00' * self.padding_size(len(self.data)))


class TraceInfoList(object):
    def __init__(self, infos=None):
        self.infos = infos or {}

    @classmethod
    def read(cls, fp):
        result = cls()
        while True:
            info = TraceInfo.read(fp)
            if info.is_end:
                break
            result.infos[info.kind] = info
        return result

    def write(self, fp):
        for _, info in sorted(self.infos.items()):
            info.write(fp)
        fp.write(TraceInfoHeaderStruct.pack(InfoKind.InfoEnd, 0))


class TraceEntry(object):
    def __init__(self, bits, pc, size, op, infos=None):
        self.bits = bits
        self.pc = pc
        self.size = size
        self.op = op
        self.infos = infos

    @property
    def is_special(self):
        return bool(self.op & TraceOp.Special)

    @staticmethod
    def struct(bits):
        return {
            32: TraceEntry32Struct,
            64: TraceEntry64Struct,
        }[bits]

    @classmethod
    def read(cls, fp, bits):
        fields = unpack_from_file(fp, cls.struct(bits))
        if not fields:
            return None

        result = cls(bits, *fields)
        if result.is_special and result.size == TraceSpecial.LoadSharedObject:
            result.infos = TraceInfoList.read(fp)
        return result

    def write(self, fp):
        fp.write(self.struct(self.bits).pack(
            self.pc, self.size, self.op
        ))
        if self.infos:
            self.infos.write(fp)


def create_exec_infos(filename, code_size=None):
    stat = os.stat(filename)
    with open(filename, 'rb') as f:
        f_contents = f.read()
    crc32 = binascii.crc32(f_contents) & 0xffffffff
    mtime = datetime.datetime.utcfromtimestamp(
        int(stat.st_mtime)
    )

    infos = [
        TraceInfo(InfoKind.ExecFileName, filename),
        TraceInfo(InfoKind.ExecFileSize, ' ' + str(stat.st_size)),
        TraceInfo(InfoKind.ExecTimeStamp, mtime.isoformat(' ')),
        TraceInfo(InfoKind.ExecFileCRC32, ' ' + str(crc32)),
    ]
    if code_size:
        infos.append(TraceInfo(InfoKind.ExecCodeSize, ' ' + str(code_size)))

    return TraceInfoList({info.kind: info for info in infos})
