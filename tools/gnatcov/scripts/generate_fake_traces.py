from __future__ import annotations


import argparse
import contextlib
import dataclasses
import enum
import os.path
import random
import sys
import time
from typing import Iterable

import SUITE.checkpointlib as ckptlib
import SUITE.srctracelib as srctracelib
from SUITE.stream_decoder import ByteStreamDecoder


parser = argparse.ArgumentParser(
    description="Generate a set of source traces that match the given SID"
    " files. The traces themselves are random. This is meant to be used to"
    " test the performance of 'gnatcov coverage'.",
)
parser.add_argument(
    "-s",
    "--seed",
    type=int,
    default=None,
    help="Seed to initialize the random number generator used to generate"
    " random traces.",
)
parser.add_argument(
    "-c",
    "--count",
    type=int,
    default=100,
    help="Number of trace files to generate.",
)
parser.add_argument(
    "-p",
    "--percentage",
    type=int,
    default=50,
    help="Percentage of bits set in the coverage buffers of consolidated"
    " generated traces.",
)

parser.add_argument(
    "output-dir",
    help="Output directory for the traces to generate.",
)
parser.add_argument(
    "sids", nargs="+", help="SID files used to generate traces."
)


@enum.unique
class UnitPart(enum.Enum):
    """Unit part, as found in a source trace entry."""

    not_applicable_part = 0
    unit_body = 1
    unit_spec = 2
    unit_separate = 3


@dataclasses.dataclass
class Buffers:
    """Coverage buffers for a given unit."""

    stmt: list[bool]
    dc: list[bool]
    mcdc: list[bool]


@dataclasses.dataclass
class Unit:
    """Description of a unit that holds coverage buffers in source traces."""

    # SID file in which this unit was found
    sid_filename: str

    # Name of the unit (as found in the source trace file)
    name: bytes

    # Source fingerprint for this unit. Used to make sure we keep at most one
    # unit per instrumented source file (i.e. we de-duplicate redundant units
    # found in multiple SID files).
    source_fingerprint: bytes

    # The following attributes reflect the trace_entry_header struct fields in
    # source trace files.

    stmt_bit_count: int
    dc_bit_count: int
    mcdc_bit_count: int

    language_kind: ckptlib.LanguageKind
    unit_part: UnitPart

    fingerprint: bytes
    bit_maps_fingerprint: bytes
    annotations_fingerprint: bytes

    @classmethod
    def from_sid(cls, filename: str) -> list[Unit]:
        """Extract the list of units from a SID file."""

        # Read the SID file and ensure it is not actually a checkpoint
        with open(filename, "rb") as f:
            ckpt = ckptlib.CheckpointFile.read(ByteStreamDecoder(f))

        if ckpt.purpose != ckptlib.CheckpointPurpose.instrumentation:
            raise ckptlib.DecodingError("SID file expected")

        # Create one Unit instance per SID_Info record recorded in this SID
        # file.
        result = []
        for cu_part, cu_id in ckpt.instrumented_unit_to_cu.items():
            match cu_part:
                case ckptlib.UnitBasedCompilationUnitPart():
                    language_kind = ckptlib.LanguageKind.unit_based
                    name = b".".join(cu_part.unit)
                    match cu_part.part:
                        case ckptlib.UnitPart.body:
                            unit_part = UnitPart.unit_body
                        case ckptlib.UnitPart.spec:
                            unit_part = UnitPart.unit_spec
                        case ckptlib.UnitPart.separate:
                            unit_part = UnitPart.unit_separate
                        case _:
                            raise AssertionError
                case ckptlib.FileBasedCompilationUnitPart():
                    language_kind = ckptlib.LanguageKind.file_based
                    name = cu_part.filename
                    unit_part = UnitPart.not_applicable_part
                case _:
                    raise AssertionError

            cu = ckpt.cu_vector[cu_id]
            for fingerprint, sidinfo in cu.sids_info.items():
                result.append(
                    Unit(
                        filename,
                        name,
                        cu.source_fingerprint,
                        len(sidinfo.stmt_bit_map),
                        len(sidinfo.dc_bit_map),
                        len(sidinfo.mcdc_bit_map),
                        language_kind,
                        unit_part,
                        fingerprint,
                        sidinfo.bit_maps_fingerprint,
                        sidinfo.annotations_fingerprint,
                    )
                )
        return result


def write_trace_file(
    filename: str,
    units: list[Unit],
    buffers: list[Buffers],
) -> None:
    """Write a source trace file for the given units/buffers.

    The lists of units and buffers must correspond (same size): each unit
    provides metadata for the corresponding set of buffers.
    """
    t = srctracelib.SrcTraceFile(
        alignment=8,
        endianity="little-endian",
        info_entries=[
            srctracelib.TraceInfoEntry("program-name", b"fake-program"),
            srctracelib.TraceInfoEntry("exec-date", b"\x00" * 8),
            srctracelib.TraceInfoEntry("user-data", b""),
        ],
        entries=[
            srctracelib.TraceEntry(
                u.language_kind.name,
                u.unit_part.name.removeprefix("unit_"),
                u.name,
                u.fingerprint,
                u.bit_maps_fingerprint,
                u.annotations_fingerprint,
                srctracelib.TraceBuffer(b.stmt),
                srctracelib.TraceBuffer(b.dc),
                srctracelib.TraceBuffer(b.mcdc),
            )
            for u, b in zip(units, buffers)
        ],
    )
    with open(filename, "wb") as f:
        t.write(f)


@contextlib.contextmanager
def show_timing(label: str) -> Iterable[None]:
    """Helper to show the time it takes to run some actions."""
    print(f"Start: {label}")
    start = time.time()
    yield
    end = time.time()
    delta = end - start
    print(f"Time ellapsed: {delta:.02f}s")
    print()


def main() -> None:
    args = parser.parse_args()
    traces_count = args.count
    percentage = min(max(args.percentage, 0), 100)
    if traces_count <= 0:
        print("No trace file to generate")
        sys.exit(1)

    # In practice, generating a huge number of different trace files in not
    # necessary to assess representative performance, and takes both a lot of
    # time + disk space. Cap the number of generated traces, and create
    # symlinks to simulate the fact that we have more traces.
    unique_traces_count = min(traces_count, 100)

    random.seed(a=args.seed)

    output_dir = os.path.abspath(getattr(args, "output-dir"))
    if not os.path.isdir(output_dir):
        os.mkdir(output_dir)

    # Find all units for which we will generate fake coverage buffers to be
    # included in the traces to create.
    units = []
    unit_by_fingerprint = {}

    def load_unit(filename: str) -> None:
        try:
            loaded_units = Unit.from_sid(filename)
        except (ckptlib.DecodingError, ValueError) as exc:
            print(f"{filename}: {exc}")
            sys.exit(1)

        for u in loaded_units:
            other_u = unit_by_fingerprint.get(u.fingerprint)

            # Ignore duplicated units, but warn when we find an inconstency
            # between two redundant units.
            if other_u is not None:
                unit_id = (u.name, u.language_kind, u.unit_part)
                other_unit_id = (
                    other_u.name,
                    other_u.language_kind,
                    other_u.unit_part,
                )
                unit_label = (
                    f"[{u.language_kind}] {u.name}/{u.unit_part}"
                    f" (from {other_u.sid_filename})"
                )
                if unit_id != other_unit_id:
                    print("Same fingerprint for different units:")
                    print(
                        f"  [{other_u.language_kind}]"
                        f" {other_u.name}/{other_u.unit_part}"
                        f" (from {other_u.sid_filename})"
                    )
                    print(f"  {unit_label}")
                elif u.source_fingerprint != other_u.source_fingerprint:
                    print(f"Different source fingerprint for {unit_id}")
                    print(f"  Other came from {other_u.sid_filename}")
                elif u.bit_maps_fingerprint != other_u.bit_maps_fingerprint:
                    print(f"Different bit maps fingerprint for {unit_id}")
                    print(f"  Other came from {other_u.sid_filename}")
                elif (
                    u.annotations_fingerprint
                    != other_u.annotations_fingerprint
                ):
                    print(f"Different annotations fingerprint for {unit_id}")
                    print(f"  Other came from {other_u.sid_filename}")
                continue

            units.append(u)
            unit_by_fingerprint[u.fingerprint] = u

    with show_timing("Load units"):
        for filename in args.sids:
            if filename.startswith("@"):
                with open(filename[1:]) as f:
                    for line in f:
                        load_unit(line.strip())
            else:
                load_unit(filename)
    print(len(units), "unique units loaded")

    # Establish a flat list of all bits in the coverage buffers to put in each
    # trace file.

    @dataclasses.dataclass(frozen=True)
    class BufferBitRef:
        unit: int
        buffer: int
        bit_id: int

    with show_timing("Create list of buffers"):
        all_buffer_bits: list[BufferBitRef] = []
        for u_id, u in enumerate(units):
            for b_id, bit_count in enumerate(
                [u.stmt_bit_count, u.dc_bit_count, u.mcdc_bit_count]
            ):
                all_buffer_bits.extend(
                    BufferBitRef(u_id, b_id, bit_id)
                    for bit_id in range(bit_count)
                )

    # Select N random bits that must be set by at least one trace in the
    # generated corpus. N depends on the coverage percentage set on the command
    # line.
    with show_timing("Select bits to set"):
        to_set_count = percentage * len(all_buffer_bits) // 100
        to_set_bits = random.sample(all_buffer_bits, to_set_count)
        assert len(to_set_bits) == to_set_count

    # Create all buffers (one per unit, and one per trace), initially filled
    # with no bit set.
    with show_timing("Create in-memory buffers"):
        traces = [
            [
                Buffers(
                    stmt=[False] * u.stmt_bit_count,
                    dc=[False] * u.dc_bit_count,
                    mcdc=[False] * u.mcdc_bit_count,
                )
                for u in units
            ]
            for _ in range(unique_traces_count)
        ]

    # Now, for each bit to set, actually flip it for at least one trace
    with show_timing("Actually set bits"):
        trace_ids = range(len(traces))
        for b in to_set_bits:
            count = random.randrange(1, len(traces))
            for t_id in random.sample(trace_ids, count):
                buffers = traces[t_id][b.unit]
                match b.buffer:
                    case 0:
                        buffers.stmt[b.bit_id] = True
                    case 1:
                        buffers.dc[b.bit_id] = True
                    case 2:
                        buffers.mcdc[b.bit_id] = True

    with show_timing("Write trace files"):

        def trace_filename(i: int) -> str:
            return os.path.join(output_dir, f"trace-{i}.srctrace")

        # Generate the unique traces
        i = 0
        for buffers in traces:
            write_trace_file(trace_filename(i), units, buffers)
            i += 1

        # Create symlinks for the other traces
        while i < traces_count:
            os.symlink(
                trace_filename(random.randrange(unique_traces_count)),
                trace_filename(i),
            )
            i += 1


if __name__ == "__main__":
    main()
