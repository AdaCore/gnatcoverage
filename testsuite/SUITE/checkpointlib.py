"""
Checkpoint/SID files loading library.
"""

from __future__ import annotations

import argparse
import dataclasses
import enum
import struct
from typing import Callable, ClassVar, TypeVar

from SUITE.stream_decoder import ByteStreamDecoder


class DecodingError(Exception):
    pass


def read_scalar(
    fp: ByteStreamDecoder,
    label: str,
    code: str,
    size: int,
) -> int:
    with fp.label_context(label):
        (value,) = struct.unpack(code, fp.read(size))
    return value


def read_u8(fp: ByteStreamDecoder, label: str) -> int:
    return read_scalar(fp, label, "B", 1)


def read_u16(fp: ByteStreamDecoder, label: str) -> int:
    return read_scalar(fp, label, "H", 2)


def read_u32(fp: ByteStreamDecoder, label: str) -> int:
    return read_scalar(fp, label, "I", 4)


def read_i32(fp: ByteStreamDecoder, label: str) -> int:
    return read_scalar(fp, label, "i", 4)


def read_bool(fp: ByteStreamDecoder, label: str) -> bool:
    return bool(read_u8(fp, label))


def read_string(fp: ByteStreamDecoder, label: str) -> bytes:
    with fp.label_context(label):
        first = read_i32(fp, "first")
        last = read_i32(fp, "last")
        length = max(0, last - first + 1)
        with fp.label_context("bytes"):
            return fp.read(length)


def read_fingerprint(fp: ByteStreamDecoder, label: str) -> bytes:
    with fp.label_context(label):
        return fp.read(20)


CollectionKey = TypeVar("CollectionKey")
CollectionElement = TypeVar("CollectionElement")


def read_vector(
    fp: ByteStreamDecoder,
    read_element: Callable[[ByteStreamDecoder, str], CollectionElement],
    label: str,
) -> list[CollectionElement]:
    with fp.label_context(label):
        length = read_i32(fp, "length")
        return [read_element(fp, "element") for _ in range(length)]


def read_set(
    fp: ByteStreamDecoder,
    read_element: Callable[[ByteStreamDecoder, str], CollectionElement],
    label: str,
) -> set[CollectionElement]:
    with fp.label_context(label):
        length = read_i32(fp, "count")
        return {read_element(fp, "element") for _ in range(length)}


def read_map(
    fp: ByteStreamDecoder,
    read_key: Callable[[ByteStreamDecoder, str], CollectionKey],
    read_element: Callable[[ByteStreamDecoder, str], CollectionElement],
    label: str,
) -> dict[CollectionKey, CollectionElement]:
    with fp.label_context(label):
        length = read_i32(fp, "count")
        result = {}
        for _ in range(length):
            key = read_key(fp, "key")
            element = read_element(fp, "element")
            result[key] = element
        return result


def read_tree(
    fp: ByteStreamDecoder,
    read_element: Callable[[ByteStreamDecoder, str], CollectionElement],
    label: str,
) -> int:
    def read_children():
        count = read_i32(fp, "count")
        for _ in range(count):
            read_subtree()

    def read_subtree():
        read_element(fp, "element")
        read_children()

    with fp.label_context(label):
        count = read_i32(fp, "count")
        if count > 0:
            read_children()

    return 0


def read_sco_range(fp: ByteStreamDecoder, label: str) -> tuple[int, int]:
    with fp.label_context(label):
        first = read_i32(fp, "first")
        last = read_i32(fp, "last")
        return (first, last)


read_aspect_id = read_u8
read_bdd_node_id = read_i32
read_bit_id = read_i32
read_condition_id = read_i32
read_cu_id = read_i32
read_pragma_id = read_u16
read_sco_id = read_i32
read_sfi = read_i32


@dataclasses.dataclass
class LocalSourceLocation:
    line: int
    column: int

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> LocalSourceLocation:
        with fp.label_context(label):
            line = read_i32(fp, "line")
            column = read_i32(fp, "column")
            return cls(line, column)


@dataclasses.dataclass
class LocalSourceLocationRange:
    first: LocalSourceLocation
    last: LocalSourceLocation

    @classmethod
    def read(
        cls, fp: ByteStreamDecoder, label: str
    ) -> LocalSourceLocationRange:
        with fp.label_context(label):
            first = LocalSourceLocation.read(fp, "first")
            last = LocalSourceLocation.read(fp, "last")
            return cls(first, last)


@dataclasses.dataclass
class SourceLocation:
    source_file: int
    l: LocalSourceLocation

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> SourceLocation:
        with fp.label_context(label):
            source_file = read_sfi(fp, "source file")
            loc = LocalSourceLocation.read(fp, "l")
            return cls(source_file, loc)


@dataclasses.dataclass
class SourceLocationRange:
    source_file: int
    l: LocalSourceLocationRange

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> SourceLocationRange:
        with fp.label_context(label):
            source_file = read_sfi(fp, "source file")
            loc = LocalSourceLocationRange.read(fp, "l")
            return cls(source_file, loc)


def read_expansion_info(fp: ByteStreamDecoder, label: str) -> None:
    # TODO: create data structures
    read_string(fp, "macro name")
    SourceLocation.read(fp, "sloc")


@enum.unique
class SCOPPKind(enum.Enum):
    in_expansion = 0
    no_expansion = 1

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> SCOPPKind:
        return SCOPPKind(read_u8(fp, label))


def read_pp_info(fp: ByteStreamDecoder, label: str) -> None:
    # TODO: create data structures
    with fp.label_context(label):
        kind = SCOPPKind.read(fp, "kind")
        LocalSourceLocationRange.read(fp, "actual source range")
        LocalSourceLocationRange.read(fp, "pp source range")
        read_vector(fp, read_expansion_info, "expansion stack")
        match kind:
            case SCOPPKind.in_expansion:
                read_expansion_info(fp, "definition loc")


def read_scope_entity(fp: ByteStreamDecoder, label: str) -> None:
    # TODO: create data structures
    with fp.label_context(label):
        SourceLocationRange.read(fp, "source range")
        read_string(fp, "name")
        LocalSourceLocation.read(fp, "sloc")

        with fp.label_context("identifier"):
            read_sfi(fp, "decl SFI")
            read_i32(fp, "decl line")


@dataclasses.dataclass
class DecisionBitInfo:
    d_sco: int
    outcome: bool

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> DecisionBitInfo:
        with fp.label_context(label):
            return cls(read_sco_id(fp, "SCO"), read_bool(fp, "outcome"))


@dataclasses.dataclass
class MCDCBitInfo:
    d_sco: int
    path_index: int

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> MCDCBitInfo:
        with fp.label_context(label):
            return cls(read_sco_id(fp, "SCO"), read_i32(fp, "path index"))


@dataclasses.dataclass
class SIDInfo:
    blocks: list[list[int]]
    stmt_bit_map: dict[int, int]
    dc_bit_map: dict[int, DecisionBitInfo]
    mcdc_bit_map: dict[int, MCDCBitInfo]
    bit_maps_fingerprint: bytes
    annotations_fingerprint: bytes

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> SIDInfo:
        with fp.label_context(label):
            blocks = read_vector(
                fp,
                lambda fp, label: read_vector(fp, read_sco_id, label),
                "blocks",
            )

            with fp.label_context("stmt bit map"):
                stmt_first = read_bit_id(fp, "stmt_first")
                stmt_last = read_bit_id(fp, "stmt_last")
                stmt_bit_map = {
                    bit: read_sco_id(fp, "SCO")
                    for bit in range(stmt_first, stmt_last + 1)
                }

            with fp.label_context("decision bit map"):
                dc_first = read_bit_id(fp, "dc_first")
                dc_last = read_bit_id(fp, "dc_last")
                dc_bit_map = {
                    bit: DecisionBitInfo.read(fp, "info")
                    for bit in range(dc_first, dc_last + 1)
                }

            with fp.label_context("MCDC bit map"):
                mcdc_first = read_bit_id(fp, "mcdc_first")
                mcdc_last = read_bit_id(fp, "mcdc_last")
                mcdc_bit_map = {
                    bit: MCDCBitInfo.read(fp, "info")
                    for bit in range(mcdc_first, mcdc_last + 1)
                }

            bit_maps_fingerprint = read_fingerprint(fp, "bit maps fingerprint")
            annotations_fingerprint = read_fingerprint(
                fp, "annotations fingerprint"
            )

            return cls(
                blocks,
                stmt_bit_map,
                dc_bit_map,
                mcdc_bit_map,
                bit_maps_fingerprint,
                annotations_fingerprint,
            )


@enum.unique
class AnyAnnotationKind(enum.Enum):
    unknown = 0
    exempt_region = 1
    exempt_on = 2
    exempt_off = 3
    dump_buffers = 4
    reset_buffers = 5
    cov_on = 6
    cov_off = 7

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> AnyAnnotationKind:
        return AnyAnnotationKind(read_u8(fp, label))


def read_ali_annotation(fp: ByteStreamDecoder, label: str) -> None:
    # TODO: create data structures
    with fp.label_context(label):
        kind = AnyAnnotationKind.read(fp, "kind")
        del kind


@enum.unique
class CheckpointPurpose(enum.Enum):
    instrumentation = 0
    consolidation = 1

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> CheckpointPurpose:
        return CheckpointPurpose(read_u8(fp, label))


@enum.unique
class BinaryTracesBits(enum.Enum):
    undetermined = 0
    bits_32 = 1
    bits_64 = 2

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> BinaryTracesBits:
        return BinaryTracesBits(read_u8(fp, label))


@enum.unique
class CoverageLevel(enum.Enum):
    insn = 0
    branch = 1
    stmt = 2
    decision = 3
    mcdc = 4
    uc_mcdc = 5
    atc = 6
    atcc = 7
    fun_call = 8
    gexpr = 9


@enum.unique
class AnyAcceptedTraceKind(enum.Enum):
    unknown = 0
    binary_trace_file = 1
    source_trace_file = 2
    llvm_trace_file = 3
    all_trace_files = 4

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> AnyAcceptedTraceKind:
        return AnyAcceptedTraceKind(read_u8(fp, label))


@dataclasses.dataclass
class CheckpointFile:
    version: int
    purpose: CheckpointPurpose
    bits: BinaryTracesBits
    levels: set[CoverageLevel]
    trace_kind: AnyAcceptedTraceKind
    files_table: FilesTable
    cu_vector: dict[int, CUInfo]
    bdd_vector: dict[int, BDDNode]
    sco_vector: dict[int, SCODescriptor | None]
    instrumented_unit_to_cu: dict[CompilationUnitPart, int]
    pp_cmds: dict[int, Command]
    sci_vector: dict[int, SourceCoverageInfo]
    trace_files: list[TraceFileElement]

    checkpoint_magic: ClassVar[bytes] = b"GNATcov checkpoint\x00"

    @staticmethod
    def read_bdd_vector(fp: ByteStreamDecoder) -> dict[int, BDDNode]:
        with fp.label_context("BDD vector"):
            result = {}
            count = read_i32(fp, "count")
            for i in range(count):
                result[i + 1] = BDDNode.read(fp, "element")
            return result

    @staticmethod
    def read_sco_vector(
        fp: ByteStreamDecoder,
    ) -> dict[int, SCODescriptor | None]:
        with fp.label_context("SCO vector"):
            result = {}
            count = read_i32(fp, "count")
            for i in range(count):
                result[i + 1] = SCODescriptor.read(fp, "element")
            return result

    @staticmethod
    def read_sci_vector(
        fp: ByteStreamDecoder,
    ) -> dict[int, SourceCoverageInfo]:
        with fp.label_context("SCI vector"):
            result = {}
            count = read_i32(fp, "count")
            for i in range(count):
                result[i + 1] = SourceCoverageInfo.read(fp, "element")
            return result

    @classmethod
    def read(cls, fp: ByteStreamDecoder) -> CheckpointFile:
        with fp.label_context("header"):
            with fp.label_context("magic"):
                magic = fp.read(len(cls.checkpoint_magic))
                if magic != cls.checkpoint_magic:
                    raise DecodingError("invalid magic")

            version = read_u32(fp, "version")
            purpose = CheckpointPurpose.read(fp, "purpose")
            bits = BinaryTracesBits.read(fp, "bits")
            with fp.label_context("levels"):
                levels = {lv for lv in CoverageLevel if read_bool(fp, str(lv))}
            trace_kind = AnyAcceptedTraceKind.read(fp, "trace kind")

        files_table = FilesTable.read(fp)
        cu_vector = {
            i + 1: cu
            for i, cu in enumerate(read_vector(fp, CUInfo.read, "CU vector"))
        }
        bdd_vector = cls.read_bdd_vector(fp)

        sco_vector = cls.read_sco_vector(fp)
        for sco_id in read_set(
            fp, read_sco_id, "uninstrumented SCOs (stmt+dc)"
        ):
            scod = sco_vector[sco_id]
            match scod:
                case StatementSCODescriptor():
                    scod.stmt_instrumented = False
                case DecisionSCODescriptor():
                    scod.decision_instrumented = False
                case _:
                    raise AssertionError

        for sco_id in read_set(fp, read_sco_id, "uninstrumented SCOs (MCDC)"):
            scod = sco_vector[sco_id]
            match scod:
                case DecisionSCODescriptor():
                    scod.decision_instrumented_for_mcdc = False
                case _:
                    raise AssertionError

        instrumented_unit_to_cu = read_map(
            fp,
            CompilationUnitPart.read,
            read_cu_id,
            "instrumented unit to CU",
        )
        pp_cmds = read_map(fp, read_sfi, Command.read, "PP commands")

        sci_vector = cls.read_sci_vector(fp)
        if purpose == CheckpointPurpose.consolidation:
            with fp.label_context("invalidated units"):
                invalidated = read_bool(fp, "invalidated")
                if not invalidated:
                    count = read_i32(fp, "count")
                    for _ in range(count):
                        CompilationUnit.read(fp, "element")

        with fp.label_context("trace files"):
            trace_files = []
            while True:
                tf = TraceFileElement.read(fp, "element")
                if tf is None:
                    break
                trace_files.append(tf)

        return CheckpointFile(
            version,
            purpose,
            bits,
            levels,
            trace_kind,
            files_table,
            cu_vector,
            bdd_vector,
            sco_vector,
            instrumented_unit_to_cu,
            pp_cmds,
            sci_vector,
            trace_files,
        )


@enum.unique
class FileKind(enum.Enum):
    stub_file = 0
    source_file = 1
    library_file = 2

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> FileKind:
        return FileKind(read_u8(fp, label))


@enum.unique
class AnyIgnoreStatus(enum.Enum):
    unknown = 0
    always = 1
    sometimes = 2
    never = 3

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> AnyIgnoreStatus:
        return AnyIgnoreStatus(read_u8(fp, label))


class FilesTable:
    def __init__(self) -> None:
        self.entries: dict[int, FilesTableEntry] = {}

    @classmethod
    def read(cls, fp: ByteStreamDecoder) -> FilesTable:
        with fp.label_context("files table"):
            first_sfi = read_i32(fp, "first sfi")
            last_sfi = read_i32(fp, "last sfi")
            result = cls()
            while True:
                entry = FilesTableEntry.read(fp)
                if entry is None:
                    break
                result.entries[entry.sfi] = entry
            assert len(result.entries) == max(last_sfi - first_sfi + 1, 0)
            return result


@dataclasses.dataclass
class FilesTableEntry:
    sfi: int
    name: bytes
    kind: FileKind
    indexed_simple_name: bool

    @classmethod
    def read(cls, fp: ByteStreamDecoder) -> FilesTableEntry | None:
        with fp.label_context("files table entry"):
            sfi = read_i32(fp, "source file index")
            if sfi == 0:
                return None
            name = read_string(fp, "name")
            kind = FileKind.read(fp, "kind")
            indexed_simple_name = read_bool(fp, "indexed_simple_name")

            # TODO: preserve extra information
            match kind:
                case FileKind.source_file:
                    ignore_status = AnyIgnoreStatus.read(fp, "ignore status")
                    unit_known = read_bool(fp, "unit known")
                    del ignore_status, unit_known
                case FileKind.library_file:
                    main_source = read_i32(fp, "main source")
                    del main_source
            return cls(sfi, name, kind, indexed_simple_name)


@dataclasses.dataclass
class CompilationUnit:
    language: LanguageKind
    unit_name: bytes

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> CompilationUnit:
        with fp.label_context(label):
            return CompilationUnit(
                LanguageKind.read(fp, "kind"), read_string(fp, "unit name")
            )


@enum.unique
class SCOProvider(enum.Enum):
    compiler = 0
    instrumenter = 1
    llvm = 2

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> SCOProvider:
        return SCOProvider(read_u8(fp, label))


@dataclasses.dataclass
class CUInfo:
    origin: int
    main_source: int
    deps: list[int]
    has_code: bool
    pp_info_map: dict[int, None]
    scope_entities: int
    ali_annotations: dict[SourceLocation, None]
    scos: list[tuple[int, int]]
    sids_info: dict[bytes, SIDInfo]

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> CUInfo:
        with fp.label_context(label):
            provider = SCOProvider.read(fp, "SCO provider")
            origin = read_i32(fp, "origin")
            main_source = read_i32(fp, "main source")
            deps = read_vector(fp, read_sfi, "deps")
            has_code = read_bool(fp, "has code")
            pp_info_map = read_map(
                fp, read_sco_id, read_pp_info, "PP info map"
            )
            scope_entities = read_tree(fp, read_scope_entity, "scope entities")
            ali_annotations = read_map(
                fp, SourceLocation.read, read_ali_annotation, "ALI annotations"
            )
            scos = read_vector(fp, read_sco_range, "scos")
            sids_info: dict[bytes, SIDInfo] = {}
            match provider:
                case SCOProvider.compiler:
                    scos_fingerprint = read_fingerprint(fp, "SCOs fingerprint")
                    return CompilerCUInfo(
                        origin,
                        main_source,
                        deps,
                        has_code,
                        pp_info_map,
                        scope_entities,
                        ali_annotations,
                        scos,
                        sids_info,
                        scos_fingerprint,
                    )
                case SCOProvider.llvm:
                    scos_fingerprint = read_fingerprint(fp, "SCOs fingerprint")
                    return LLVMCUInfo(
                        origin,
                        main_source,
                        deps,
                        has_code,
                        pp_info_map,
                        scope_entities,
                        ali_annotations,
                        scos,
                        sids_info,
                        scos_fingerprint,
                    )
                case SCOProvider.instrumenter:
                    sids_info = read_map(
                        fp, read_fingerprint, SIDInfo.read, "SIDs info"
                    )
                    source_fingerprint = read_fingerprint(
                        fp, "source fingerprint"
                    )
                    return InstrumenterCUInfo(
                        origin,
                        main_source,
                        deps,
                        has_code,
                        pp_info_map,
                        scope_entities,
                        ali_annotations,
                        scos,
                        sids_info,
                        source_fingerprint,
                    )


@dataclasses.dataclass
class BaseCompilerCUInfo(CUInfo):
    scos_fingerprint: bytes


@dataclasses.dataclass
class CompilerCUInfo(BaseCompilerCUInfo):
    pass


@dataclasses.dataclass
class LLVMCUInfo(BaseCompilerCUInfo):
    pass


@dataclasses.dataclass
class InstrumenterCUInfo(CUInfo):
    source_fingerprint: bytes


@enum.unique
class BDDNodeKind(enum.Enum):
    outcome = 0
    condition = 1
    jump = 2

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> BDDNodeKind:
        return BDDNodeKind(read_u8(fp, label))


@dataclasses.dataclass
class BDDNode:

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> BDDNode:
        with fp.label_context(label):
            kind = BDDNodeKind.read(fp, "kind")
            match kind:
                case BDDNodeKind.outcome:
                    return OutcomeBDDNode(read_bool(fp, "decision outcome"))

                case BDDNodeKind.condition:
                    return ConditionBDDNode(
                        read_bdd_node_id(fp, "parent"),
                        read_bool(fp, "parent value"),
                        read_sco_id(fp, "condition SCO"),
                        (
                            read_bdd_node_id(fp, "dest:false"),
                            read_bdd_node_id(fp, "dest:true"),
                        ),
                        read_i32(fp, "path offset"),
                    )

                case BDDNodeKind.jump:
                    return JumpBDDNode(read_bdd_node_id(fp, "dest"))


@dataclasses.dataclass
class OutcomeBDDNode(BDDNode):
    decision_outcome: bool


@dataclasses.dataclass
class ConditionBDDNode(BDDNode):
    parent: int
    parent_value: bool
    c_sco: int
    dests: tuple[int, int]
    path_offset: int


@dataclasses.dataclass
class JumpBDDNode(BDDNode):
    dest: int


@dataclasses.dataclass
class BDDType:
    decision: int
    root_condition: int
    first_node: int
    last_node: int
    first_multipath_condition: int
    reachable_outcomes: tuple[bool, bool]
    path_count: int

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> BDDType:
        with fp.label_context(label):
            return BDDType(
                read_sco_id(fp, "decision"),
                read_bdd_node_id(fp, "root condition"),
                read_bdd_node_id(fp, "first node"),
                read_bdd_node_id(fp, "last node"),
                read_bdd_node_id(fp, "first multipath condition"),
                (
                    read_bool(fp, "reachable outcomes:false"),
                    read_bool(fp, "reachable outcomes:true"),
                ),
                read_i32(fp, "path count"),
            )


@enum.unique
class SCOKind(enum.Enum):
    removed = 0
    statement = 1
    decision = 2
    condition = 3
    operator = 4
    fun = 5
    call = 6
    guarded_expr = 7

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> SCOKind:
        return SCOKind(read_u8(fp, label))


@enum.unique
class StatementKind(enum.Enum):
    no_statement = 0

    type_declaration = 1
    subtype_declaration = 2
    object_declaration = 3
    renaming_declaration = 4
    generic_instantiation = 5
    other_declaration = 6

    accept_statement = 7
    case_statement = 8
    exit_statement = 9
    for_loop_statement = 10
    if_statement = 11
    pragma_statement = 12
    disabled_pragma_statement = 13
    extended_return_statement = 14
    select_statement = 15
    while_loop_statement = 16
    degenerate_subprogram_statement = 17
    call_stmt = 18
    call_expr = 19
    other_statement = 20

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> StatementKind:
        return StatementKind(read_u8(fp, label))


@enum.unique
class DecisionKind(enum.Enum):
    if_statement = 0
    exit_statement = 1
    entry_guard = 2
    pragma_decision = 3
    while_loop = 4
    expression = 5
    aspect = 6

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> DecisionKind:
        return DecisionKind(read_u8(fp, label))


@enum.unique
class OperatorKind(enum.Enum):
    op_not = 0
    op_and_then = 1
    op_or_else = 2

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> OperatorKind:
        return OperatorKind(read_u8(fp, label))


@enum.unique
class Tristate(enum.Enum):
    false = 0
    true = 1
    unknown = 2

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> Tristate:
        return Tristate(read_u8(fp, label))


@dataclasses.dataclass
class SCODescriptor:
    origin: int
    sloc_range: SourceLocationRange
    parent: int

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> SCODescriptor | None:
        with fp.label_context(label):
            kind = SCOKind.read(fp, "kind")
            if kind == SCOKind.removed:
                return None

            origin = read_cu_id(fp, "origin")
            sloc_range = SourceLocationRange.read(fp, "sloc range")
            parent = read_sco_id(fp, "parent")

            match kind:
                case SCOKind.statement:
                    return StatementSCODescriptor(
                        origin,
                        sloc_range,
                        parent,
                        StatementKind.read(fp, "statement kind"),
                        read_sco_id(fp, "dominant"),
                        Tristate.read(fp, "dominant value"),
                        SourceLocation.read(fp, "dominant sloc"),
                        SourceLocationRange.read(fp, "handler range"),
                        read_pragma_id(fp, "pragma name"),
                    )

                case SCOKind.condition:
                    return ConditionSCODescriptor(
                        origin,
                        sloc_range,
                        parent,
                        Tristate.read(fp, "value"),
                        # TODO: handle 64-bit
                        read_set(fp, read_u32, "PC set"),
                        read_bdd_node_id(fp, "BDD node"),
                        read_condition_id(fp, "index"),
                    )

                case SCOKind.decision:
                    return DecisionSCODescriptor(
                        origin,
                        sloc_range,
                        parent,
                        read_sco_id(fp, "expression"),
                        DecisionKind.read(fp, "decision kind"),
                        SourceLocation.read(fp, "control location"),
                        read_condition_id(fp, "last condition index"),
                        BDDType.read(fp, "decision BDD"),
                        read_bool(fp, "degraded origins"),
                        read_aspect_id(fp, "aspect name"),
                        read_i32(fp, "path count"),
                    )

                case SCOKind.operator:
                    read_u8(fp, "first")
                    read_u8(fp, "last")
                    return OperatorSCODescriptor(
                        origin,
                        sloc_range,
                        parent,
                        (
                            read_sco_id(fp, "left operand"),
                            read_sco_id(fp, "right operand"),
                        ),
                        OperatorKind.read(fp, "operator kind"),
                    )

                case SCOKind.fun | SCOKind.call:
                    return FunCallSCODescriptor(
                        origin,
                        sloc_range,
                        parent,
                        read_bool(fp, "is expression"),
                        read_bool(fp, "fun call instrumented"),
                    )

                case SCOKind.guarded_expr:
                    return GuardedExprSCODescriptor(
                        origin,
                        sloc_range,
                        parent,
                        read_bool(fp, "gexpr instrumented"),
                    )

                case _:
                    raise AssertionError


@dataclasses.dataclass
class StatementSCODescriptor(SCODescriptor):
    s_kind: StatementKind
    dominant: int
    dominant_value: Tristate
    dominant_sloc: SourceLocation
    handler_range: SourceLocationRange
    pragma_name: int
    stmt_instrumented: bool = True


@dataclasses.dataclass
class ConditionSCODescriptor(SCODescriptor):
    value: Tristate
    pc_set: set[int]
    bdd_node: int
    index: int


@dataclasses.dataclass
class DecisionSCODescriptor(SCODescriptor):
    expression: int
    d_kind: DecisionKind
    control_location: SourceLocation
    last_cond_index: int
    decision_bdd: BDDType
    degraded_origins: bool
    aspect_name: int
    path_count: int
    decision_instrumented: bool = True
    decision_instrumented_for_mcdc: bool = True


@dataclasses.dataclass
class OperatorSCODescriptor(SCODescriptor):
    operands: tuple[int, int]
    op_kind: OperatorKind


@dataclasses.dataclass
class FunCallSCODescriptor(SCODescriptor):
    is_expr: bool
    fun_call_instrumented: bool


@dataclasses.dataclass
class GuardedExprSCODescriptor(SCODescriptor):
    gexpr_instrumented: bool


@enum.unique
class LanguageKind(enum.Enum):
    unit_based = 0
    file_based = 1

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> LanguageKind:
        return LanguageKind(read_u32(fp, label))


@enum.unique
class UnitPart(enum.Enum):
    body = 0
    spec = 1
    separate = 2

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> UnitPart:
        return UnitPart(read_u8(fp, label))


@dataclasses.dataclass(frozen=True)
class CompilationUnitPart:

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> CompilationUnitPart:
        with fp.label_context(label):
            kind = LanguageKind.read(fp, "kind")
            match kind:
                case LanguageKind.unit_based:
                    return UnitBasedCompilationUnitPart(
                        tuple(read_vector(fp, read_string, "unit")),
                        UnitPart.read(fp, "part"),
                    )
                case LanguageKind.file_based:
                    return FileBasedCompilationUnitPart(
                        read_string(fp, "filename")
                    )
                case _:
                    raise AssertionError


@dataclasses.dataclass(frozen=True)
class UnitBasedCompilationUnitPart(CompilationUnitPart):
    unit: tuple[bytes, ...]
    part: UnitPart


@dataclasses.dataclass(frozen=True)
class FileBasedCompilationUnitPart(CompilationUnitPart):
    filename: bytes


@dataclasses.dataclass
class Command:
    command: bytes
    arguments: list[bytes]
    environment: dict[bytes, bytes]

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> Command:
        with fp.label_context(label):
            return Command(
                read_string(fp, "command"),
                read_vector(fp, read_string, "arguments"),
                read_map(fp, read_string, read_string, "environment"),
            )


@enum.unique
class LineState(enum.Enum):
    not_covered = 0
    partially_covered = 1
    covered = 2
    no_code = 3
    not_coverable = 4
    undetermined_coverage = 5
    disabled_coverage = 6
    exempted_with_violation = 7
    exempted_with_undetermined_cov = 8
    exempted_no_violation = 9

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> LineState:
        return LineState(read_u8(fp, label))


@dataclasses.dataclass
class SourceCoverageInfo:
    state: dict[CoverageLevel, LineState]

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> SourceCoverageInfo:
        with fp.label_context(label):
            kind = SCOKind.read(fp, "kind")
            states = {lv: LineState.read(fp, str(lv)) for lv in CoverageLevel}
            match kind:
                case SCOKind.statement:
                    return StatementSourceCoverageInfo(
                        states,
                        read_bool(fp, "basic block has code"),
                        read_bool(fp, "executed"),
                        read_bool(fp, "line executed"),
                    )
                case SCOKind.decision:
                    return DecisionSourceCoverageInfo(
                        states,
                        (
                            read_bool(fp, "outcome_taken:false"),
                            read_bool(fp, "outcome_taken:true"),
                        ),
                        (
                            read_bool(fp, "known_outcome_taken:false"),
                            read_bool(fp, "known_outcome_taken:true"),
                        ),
                        read_set(fp, Evaluation.read, "evaluations"),
                    )
                case SCOKind.fun | SCOKind.call:
                    return FunCallSourceCoverageInfo(
                        states, read_bool(fp, "fun call executed")
                    )
                case SCOKind.guarded_expr:
                    return GuardedExprSourceCoverageInfo(
                        states, read_bool(fp, "gexpr executed")
                    )
                case _:
                    return SourceCoverageInfo(states)


@dataclasses.dataclass
class StatementSourceCoverageInfo(SourceCoverageInfo):
    basic_block_has_code: bool
    executed: bool
    line_executed: bool


@dataclasses.dataclass
class Evaluation:
    decision: int
    values: dict[int, Tristate]
    outcome: Tristate
    next_condition: int

    @classmethod
    def read(cls, fp: ByteStreamDecoder, label: str) -> Evaluation:
        with fp.label_context(label):
            decision = read_sco_id(fp, "decision")

            with fp.label_context("values"):
                values = {}
                count = read_i32(fp, "count")
                for i in range(count):
                    values[i + 1] = Tristate.read(fp, "element")

            outcome = Tristate.read(fp, "outcome")
            next_condition = read_condition_id(fp, "next condition")

            return Evaluation(decision, values, outcome, next_condition)


@dataclasses.dataclass
class DecisionSourceCoverageInfo(SourceCoverageInfo):
    outcome_taken: tuple[bool, bool]
    known_outcome_taken: tuple[bool, bool]
    evaluations: set


@dataclasses.dataclass
class FunCallSourceCoverageInfo(SourceCoverageInfo):
    fun_call_executed: bool


@dataclasses.dataclass
class GuardedExprSourceCoverageInfo(SourceCoverageInfo):
    gexpr_executed: bool


@dataclasses.dataclass
class TraceFileElement:
    filename: bytes
    kind: AnyAcceptedTraceKind
    context: bytes
    program_name: bytes
    time: bytes
    user_data: bytes

    @classmethod
    def read(
        cls,
        fp: ByteStreamDecoder,
        label: str,
    ) -> TraceFileElement | None:
        with fp.label_context(label):
            name = read_string(fp, "name")
            if not name:
                return None

            return TraceFileElement(
                name,
                AnyAcceptedTraceKind.read(fp, "kind"),
                read_string(fp, "context"),
                read_string(fp, "program name"),
                read_string(fp, "time"),
                read_string(fp, "user data"),
            )


parser = argparse.ArgumentParser("Decode a checkpoint file")
parser.add_argument(
    "--debug", "-d", action="store_true", help="Enable debug traces"
)
parser.add_argument("checkpoint-file", help="Checkpoint file to decode")


if __name__ == "__main__":
    args = parser.parse_args()
    with open(getattr(args, "checkpoint-file"), "rb") as f:
        ckpt = CheckpointFile.read(ByteStreamDecoder(f, args.debug, 4))
