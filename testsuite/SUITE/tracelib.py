"""
Qemu trace files loading and saving library.
"""


import binascii
import datetime
import os
from struct import Struct


class Enum(object):
    """
    Base class to declare enumerations.
    """

    RANGE = None
    """
    Tuple that contains the first and the last enumerated values. We consider
    that all integers between these bounds (included) are valid enumerated
    values.

    :type: (int, int)
    """

    @classmethod
    def in_range(cls, value):
        """
        Return whether `value` is in `cls`'s range.
        """
        low, high = cls.RANGE
        return low <= value <= high

    @classmethod
    def check(cls, value):
        """
        Assert that `value` is in `cls`'s range.
        """
        assert cls.in_range(value), 'Invalid {}: {} not in {} .. {}'.format(
            cls.__name__,
            value,
            cls.RANGE[0], cls.RANGE[1]
        )


class TraceKind(Enum):
    """
    Kind of trace file. See Trace_Kind in qemu_traces.ads.
    """
    Flat = 0
    History = 1
    Info = 2
    DecisionMap = 3

    RANGE = (0, 3)


class InfoKind(Enum):
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

    RANGE = (0, 10)


class TraceOp(Enum):
    """
    Trace operation bitmasks. See Trace_Op_* in qemu_traces.ads.
    """
    Block = 0x10
    Fault = 0x20
    Br0 = 0x01
    Br1 = 0x02
    Special = 0x80

    # Helper for the format_flags method, below
    flag_chars = [(Block,   'B'), (Fault, 'F'),
                  (Br0,     'b'), (Br1,   'f'),
                  (Special, 's')]

    @classmethod
    def format_flags(cls, flags):
        """
        Return a human-readable representation for the given TraceOp value.

        This returns a compact representation: "-----" for no bit set, "B-bf-"
        for the block, br0 and br1 bits set, etc. This compact representation
        has always the same length, and is thus suitable for tabular printings.

        :param int flags: Integer value for the TraceOp to decode. This is the
            integer that is read directly from the trace file.
        """
        return ''.join(char if flags & v else '-'
                       for v, char in cls.flag_images)


class TraceSpecial(Enum):
    """
    Special trace operations. See Trace_Special_* in qemu_traces.ads.
    """
    Loadaddr = 1
    LoadSharedObject = 2
    UnloadSharedObject = 3

    RANGE = (1, 3)


TRACE_MAGIC = '#QEMU-Traces'
"""
Exepected value of the Magic header field. See Qemu_Trace_Magic in
qemu_traces.ads.
"""


def create_trace_header(kind, pc_size, big_endian, machine):
    """
    Return a tuple to represent a trace header. See Trace_Header in
    qemu_traces.ads.

    :param int kind: Trace file kind. See TraceKind.
    :param int pc_size: Size of Program Counter on target, in bytes.
    :param bool big_endian: Whether the target is big-endian.
    :param int machine: Target ELF machine ID.
    """
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
    """
    Read all the bytes necessary to decode `struct` from the `fp` file and
    return the decoded structure as a tuple.

    :param file fp: File from which to read bytes.
    :param Struct struct: Struct instance to decode data.
    """
    buf = fp.read(struct.size)
    if not buf:
        return None
    assert len(buf) == struct.size
    return struct.unpack(buf)


class TraceFile(object):
    """
    In-memory representation of a trace file.
    """

    def __init__(self, first_header, infos, second_header, entries):
        """
        Create a trace file.

        If this instance must represent a partial trace file, `second_header`
        must be None and `entries` must be empty.

        :param first_header: Tuple for a TraceHeaderStruct structure.
        :param TraceInfoList infos: Information for the traced program.
        :param second_header: Tuple for a TraceHeaderStruct structure.
        :param TraceInfoList entries: Trace entries.
        """
        self.first_header = first_header
        assert self.first_header

        self.bits = TraceFile.bits(first_header)
        self.infos = infos

        self.second_header = second_header

        # If there is no second header, make sure we have no entry
        if not second_header:
            assert not entries
            return

        self.entries = entries
        for entry in entries:
            assert entry.bits == self.bits

    @staticmethod
    def bits(header):
        """
        Return size of PC on target, in bits.
        """
        assert header[3] in (4, 8)
        return header[3] * 8

    @classmethod
    def read(cls, fp):
        """
        Read a trace file from the `fp` file. Return a TraceFile instance.
        """
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
        """
        Write this trace file to the `fp` file.
        """
        assert self.bits

        fp.write(TraceHeaderStruct.pack(*self.first_header))
        self.infos.write(fp)
        fp.write(TraceHeaderStruct.pack(*self.second_header))
        for entry in self.entries:
            entry.write(fp)

    def iter_entries(self, raw=False):
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
                    loadaddr.is_special and
                    loadaddr.size == TraceSpecial.Loadaddr
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


class TraceInfo(object):
    """
    In-memory representation for a trace info entry.
    """

    def __init__(self, kind, data):
        """
        Create a trace info entry.

        :param int kind: Trace info entry kind. See InfoKind.
        :param str data: Raw data (bytes) for this entry.
        """
        InfoKind.check(kind)
        self.kind = kind
        self.data = data

    @staticmethod
    def padding_size(data_size):
        """
        Number of padding bytes at the end of the Trace_Info record.
        """
        return 0 if data_size % 4 == 0 else (4 - (data_size % 4))

    @property
    def is_end(self):
        """
        Whether this trace info entry is the trace info list end marker.
        """
        return self.kind == InfoKind.InfoEnd

    @classmethod
    def read(cls, fp):
        """
        Read a trace info entry from the `fp` file. Return a TraceInfo
        instance.
        """
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
        """
        Write this trace info to the `fp` file.
        """
        fp.write(TraceInfoHeaderStruct.pack(self.kind, len(self.data)))
        fp.write(self.data)
        fp.write('\x00' * self.padding_size(len(self.data)))


class TraceInfoList(object):
    """
    In-memory representation of a list of list of trace info entries.
    """

    def __init__(self, infos=None):
        """
        Create a list of trace info entries.

        :param dict[int, TraceInfo]|None infos: None for an empty list.
            Otherwise, mapping from trace info kind (see InfoKind) to TraceInfo
            instance.
        """
        self.infos = infos or {}

    @classmethod
    def read(cls, fp):
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

    def write(self, fp):
        """
        Write this trace info list to the `fp` file.
        """
        for _, info in sorted(self.infos.items()):
            info.write(fp)
        fp.write(TraceInfoHeaderStruct.pack(InfoKind.InfoEnd, 0))


class TraceEntry(object):
    """
    In-memory representation of a trace entry.
    """

    def __init__(self, bits, pc, size, op, infos=None):
        """
        :param int bits: Number of bits in the target PC.
        :param int pc: PC for the trace entry (i.e. first address to be
            executed, or address of the event).
        :param int size: Size in bytes for the trace entry.
        :param None|TraceInfoList infos: Informations associated to this trace
            entry.
        """
        self.bits = bits
        self.pc = pc
        self.size = size
        self.op = op
        self.infos = infos

    @property
    def is_special(self):
        """
        Whether this is a special trace entry. See Trace_Op_Special in
        qemu_traces.ads.
        """
        return bool(self.op & TraceOp.Special)

    @staticmethod
    def struct(bits):
        """
        Struct instance to decode a trace entry given the number of bits in PC
        PC on a target.
        """
        return {
            32: TraceEntry32Struct,
            64: TraceEntry64Struct,
        }[bits]

    @classmethod
    def read(cls, fp, bits):
        """
        Read a trace entry from the `fp` file. Return a TraceEntry instance.
        """
        fields = unpack_from_file(fp, cls.struct(bits))
        if not fields:
            return None

        result = cls(bits, *fields)
        if result.is_special and result.size == TraceSpecial.LoadSharedObject:
            result.infos = TraceInfoList.read(fp)
        return result

    def write(self, fp):
        """
        Write this trace entry to the `fp` file.
        """
        fp.write(self.struct(self.bits).pack(
            self.pc, self.size, self.op
        ))
        if self.infos:
            self.infos.write(fp)


def create_exec_infos(filename, code_size=None):
    """
    Create a TraceInfoList object to describe the given executable.

    :param str filename: Path to the executable file.
    :param int|None code_size: If provided, size of the executable code section
        in `filename`. Used to create the Exec_Code_Size trace information
        entry.
    :rtype: TraceInfoList
    """
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
