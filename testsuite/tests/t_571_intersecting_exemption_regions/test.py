"""
Check that gnatcov emits the expected warning in case on overlapping exemption
regions.
"""

from __future__ import annotations

from collections.abc import Iterator
import contextlib
import dataclasses
import os.path

from e3.fs import mkdir

from SCOV.minicheck import (
    CovData,
    CovReport,
    build_and_run,
    check_xcov_reports,
)
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import (
    Exempt_Off,
    Exempt_On,
    Ext_Annotation,
    generate_annotations,
    gprfor,
    xcov,
)


srcdir = os.getcwd()


def srcfile(filename: str) -> str:
    """
    Helper to compute the absolute path to a source file, regardless of what
    the current working directory is.
    """
    return os.path.join(srcdir, filename)


@contextlib.contextmanager
def build_and_run_wrapper(
    label: str,
    mains: list[str],
    ext_annotations: list[Ext_Annotation],
    tolerate_instrument_messages: str | None = None,
) -> Iterator[list[str]]:
    """
    Helper to build and run a test project in a dedicated temporary directory,
    and let the caller call "gnatcov coverage".
    """
    thistest.log(f"== {label} ==")
    tmp = Wdir(f"tmp_{label}")
    units = [
        os.path.splitext(m)[0] if m.endswith(".adb") else m for m in mains
    ]
    mains = [os.path.splitext(m)[0] for m in mains]
    yield build_and_run(
        gprsw=GPRswitches(
            root_project=gprfor(srcdirs=[".."], mains=mains), units=units
        ),
        covlevel="stmt",
        mains=mains,
        extra_instr_args=[
            "--external-annotations",
            generate_annotations(ext_annotations),
        ],
        extra_coverage_args=[],
        tolerate_instrument_messages=tolerate_instrument_messages,
    )
    tmp.to_homedir()


def single_check(
    label: str,
    mains: list[str],
    ext_annotations: list[Ext_Annotation],
    warnings: list[str],
    expected_cov: CovReport,
) -> None:
    with build_and_run_wrapper(
        label,
        mains,
        ext_annotations,
        tolerate_instrument_messages=".*",
    ) as xcov_args:
        thistest.fail_if_not_equal(
            '"gnatcov instrument" output',
            "\n".join(warnings),
            contents_of("instrument.log").strip(),
        )

        xcov(xcov_args + ["--annotate=xcov", "--output-dir=xcov"])
        check_xcov_reports("xcov", expected_cov)


single_check(
    label="basic",
    mains=["exempt_nested.adb", "lone_exempt_off.adb"],
    ext_annotations=[],
    warnings=[
        # All nested exemptions should be ignored with a warning
        f"*** exempt_nested.adb:{sloc}: warning: Ignoring annotation"
        " EXEMPT_ON: nested regions are ignored"
        for sloc in ["8:4", "10:7", "16:4"]
    ]
    + [
        # Lone Exempt_Off annotations (i.e. without a corresponding Exempt_On)
        # should be reported.
        "*** lone_exempt_off.adb:4:4: warning: EXEMPT_OFF annotation found"
        " without a corresponding EXEMPT_ON",
    ],
    expected_cov={
        "exempt_nested.adb.xcov": {
            "#": set(range(5, 24)) | set(range(25, 28))
        },
        "lone_exempt_off.adb.xcov": {"+": {3, 5}},
    },
)

single_check(
    label="in_ext_independence",
    mains=["lone_exempt_off.adb", "lone_exempt_on.adb"],
    ext_annotations=[
        Exempt_On(srcfile("lone_exempt_off.adb"), "3:4", "3:4", "J"),
        Exempt_Off(srcfile("lone_exempt_on.adb"), "5:10", "5:10"),
    ],
    warnings=[
        # Even though the external annotation puts an Exempt_On just before the
        # Exempt_Off in the source file, external annotations and in-source
        # annotations should be processed separately, so these annotations are
        # not paired, and so Exempt_Off is discarded, while Exempt_On creates a
        # region that goes until the last SCO in that file.
        "*** lone_exempt_off.adb:4:4: warning: EXEMPT_OFF annotation found"
        " without a corresponding EXEMPT_ON",
        # Likewise, but with the Exempt_On in the source code and the
        # Exempt_Off in the external annotations.
        "*** lone_exempt_on.adb:5:10: warning: EXEMPT_OFF annotation found"
        " without a corresponding EXEMPT_ON",
    ],
    expected_cov={
        "lone_exempt_off.adb.xcov": {"#": {3, 4, 5}},
        "lone_exempt_on.adb.xcov": {"+": {3}, "#": {4, 5}},
    },
)

# Check that gnatcov correctly processes sequences of annotations that span
# over multiple files. Currently, this can happen only with C sources: headers
# are instrumented as part of the instrumentation of the bodies, and so we end
# up with annotations sets that mix multiple files.
single_check(
    label="multi_files",
    mains=["dummy.c"],
    ext_annotations=[],
    warnings=[],
    expected_cov={
        "dummy.c.xcov": {"+": {6, 10}, "#": {7, 8, 9}},
        "dummy.h.xcov": {"#": {4, 5, 6}},
    },
)

# Check various overlapping settings between an exemption region set during
# instrumentation and an exemption region set during "gnatcov coverage". For
# efficiency, build and run the test project only once.
with build_and_run_wrapper(
    label="overlapping",
    mains=["exempt_overlap.adb"],
    ext_annotations=[
        Exempt_On(srcfile("exempt_overlap.adb"), "7:4", "7:4", "Base"),
        Exempt_Off(srcfile("exempt_overlap.adb"), "11:4", "11:4"),
    ],
) as xcov_args:

    @dataclasses.dataclass
    class OverlapTest:
        sublabel: str
        sloc_start: str
        sloc_end: str
        warnings: list[str]
        expected_cov: CovData

        @staticmethod
        def for_overlap(
            sublabel: str,
            sloc_start: str,
            sloc_end: str,
        ) -> OverlapTest:
            return OverlapTest(
                sublabel,
                sloc_start,
                sloc_end,
                [
                    f"*** exempt_overlap.adb:{sloc_start}: warning: Ignoring"
                    " annotation EXEMPT_ON that intersects with region"
                    " starting at exempt_overlap.adb:7:4",
                ],
                {"#": {7, 8, 9, 10, 11}, "+": {5, 13}},
            )

    for otc in [
        OverlapTest(
            "disj_before1",
            "4:1",
            "6:1",
            [],
            {"#": {4, 5, 6, 7, 8, 9, 10, 11}, "+": {13}},
        ),
        OverlapTest(
            "disj_before2",
            "4:1",
            "7:3",
            [],
            {"#": {4, 5, 6, 7, 8, 9, 10, 11}, "+": {13}},
        ),
        OverlapTest.for_overlap("over_before1", "4:1", "7:5"),
        OverlapTest.for_overlap("over_before2", "4:1", "13:1"),
        OverlapTest.for_overlap("over_inside", "9:1", "10:1"),
        OverlapTest.for_overlap("over_after1", "9:1", "10:1"),
        OverlapTest(
            "dis_after1",
            "11:39",
            "14:1",
            [],
            {"+": {5}, "#": {7, 8, 9, 10, 11, 12, 13, 14}},
        ),
        OverlapTest(
            "dis_after2",
            "12:1",
            "14:1",
            [],
            {"+": {5}, "#": {7, 8, 9, 10, 11, 12, 13, 14}},
        ),
    ]:
        thistest.log(f"== overlapping: {otc.sublabel} ==")
        output_dir = f"xcov-{otc.sublabel}"
        log_file = f"cov-{otc.sublabel}.txt"
        annotation_file = generate_annotations(
            [
                Exempt_On(
                    srcfile("exempt_overlap.adb"),
                    otc.sloc_start,
                    otc.sloc_start,
                    "J",
                ),
                Exempt_Off(
                    srcfile("exempt_overlap.adb"), otc.sloc_end, otc.sloc_end
                ),
            ],
            subdir=f"ann-{otc.sublabel}",
        )
        xcov(
            xcov_args
            + [
                "--annotate=xcov",
                f"--output-dir={output_dir}",
                f"--external-annotations={annotation_file}",
            ],
            out=log_file,
            tolerate_messages=".*",
        )
        thistest.fail_if_not_equal(
            '"gnatcov coverage" output',
            "\n".join(otc.warnings),
            contents_of(log_file).strip(),
        )
        check_xcov_reports(
            output_dir, {"exempt_overlap.adb.xcov": otc.expected_cov}
        )

# Check various overlapping settings between exemptions contributed by various
# checkpoints during consolidation. For efficiency, build and run the test
# project only once.
with build_and_run_wrapper(
    label="ckpt",
    mains=["exempt_overlap.adb"],
    ext_annotations=[],
) as xcov_args:

    @dataclasses.dataclass
    class CkptTest:
        sublabel: str
        sloc_start1: str
        sloc_end1: str | None
        sloc_start2: str
        sloc_end2: str | None
        warnings: list[str]
        expected_cov: CovData
        justif2: str = "J"

    for ctc in [
        # Disjoint regions: no warning and both regions exempted
        CkptTest(
            "disjoint1",
            "4:1",
            "6:1",
            "10:1",
            "12:1",
            [],
            {"#": {4, 5, 6, 10, 11, 12}, "+": {9, 13}},
        ),
        # Sloc bounds are inclusive: there is no intersection when they are
        # adjacent.
        CkptTest(
            "disjoint2",
            "5:1",
            "5:5",
            "5:6",
            "5:10",
            [],
            {"#": {5}, "+": {7, 9, 11, 13}},
        ),
        CkptTest(
            "disjoint3",
            "5:6",
            "5:10",
            "5:1",
            "5:5",
            [],
            {"#": {5}, "+": {7, 9, 11, 13}},
        ),
        # Overlapping (no common bound): warning and only the first region
        # Overlapping (no common bound): warning and only the first region
        # exempted.
        CkptTest(
            "overlap1",
            "4:1",
            "8:1",
            "6:1",
            "12:1",
            [
                "*** exempt_overlap.adb:6:1: warning: Ignoring annotation"
                " EXEMPT_ON that intersects with region starting at"
                " exempt_overlap.adb:4:1",
            ],
            {"#": {4, 5, 6, 7, 8}, "+": {9, 11, 13}},
        ),
        # Overlapping (lower bound common): warning and only the first region
        # exempted.
        CkptTest(
            "overlap2",
            "4:1",
            "6:1",
            "4:1",
            "8:1",
            [
                "*** exempt_overlap.adb:4:1: warning: Ignoring annotation"
                " EXEMPT_ON that intersects with region starting at"
                " exempt_overlap.adb:4:1",
            ],
            {"#": {4, 5, 6}, "+": {7, 9, 11, 13}},
        ),
        # Overlapping (upper bound common): warning and only the first region
        # exempted.
        CkptTest(
            "overlap3",
            "4:1",
            "6:1",
            "2:1",
            "6:1",
            [
                "*** exempt_overlap.adb:2:1: warning: Ignoring annotation"
                " EXEMPT_ON that intersects with region starting at"
                " exempt_overlap.adb:4:1",
            ],
            {"#": {4, 5, 6}, "+": {7, 9, 11, 13}},
        ),
        # Overlapping (equal bounds, same justification): no warning, the
        # region is exempted.
        CkptTest(
            "equal",
            "4:1",
            "8:1",
            "4:1",
            "8:1",
            [],
            {"#": {4, 5, 6, 7, 8}, "+": {9, 11, 13}},
        ),
        # Overlapping (equal bounds, different justification): warning about
        # the justification discrepancy, the region is exempted.
        CkptTest(
            "justif_diff",
            "10:1",
            None,
            "10:1",
            None,
            [
                "*** exempt_overlap.adb:10:1: warning: Duplicate exemption"
                " region",
                "*** exempt_overlap.adb:10:1: warning: Discarding"
                " justification: J2",
                "*** exempt_overlap.adb:10:1: warning: In favor of: J",
            ],
            {"+": {5, 7, 9}, "#": {10, 11, 12, 13}},
            "J2",
        ),
        # Sloc bounds are inclusive: regions intersect when one's start bound
        # is equal to the other's end bound.
        CkptTest(
            "overlap4",
            "5:1",
            "5:5",
            "5:5",
            "5:10",
            [
                "*** exempt_overlap.adb:5:5: warning: Ignoring annotation"
                " EXEMPT_ON that intersects with region starting at"
                " exempt_overlap.adb:5:1",
            ],
            {"#": {5}, "+": {7, 9, 11, 13}},
        ),
        CkptTest(
            "overlap5",
            "5:5",
            "5:10",
            "5:1",
            "5:5",
            [
                "*** exempt_overlap.adb:5:1: warning: Ignoring annotation"
                " EXEMPT_ON that intersects with region starting at"
                " exempt_overlap.adb:5:5",
            ],
            {"#": {5}, "+": {7, 9, 11, 13}},
        ),
    ]:
        thistest.log(f"== ckpt: {ctc.sublabel} ==")

        # Create the two checkpoints with the requested exemption regions
        checkpoints = []
        for index, sloc_start, sloc_end, justif in [
            (1, ctc.sloc_start1, ctc.sloc_end1, "J"),
            (2, ctc.sloc_start2, ctc.sloc_end2, ctc.justif2),
        ]:
            checkpoints.append(f"{ctc.sublabel}-c{index}.ckpt")
            ann_list: list[Ext_Annotation] = [
                Exempt_On(
                    srcfile("exempt_overlap.adb"),
                    sloc_start,
                    sloc_end,
                    justif,
                ),
            ]
            if sloc_end:
                ann_list.append(
                    Exempt_Off(
                        srcfile("exempt_overlap.adb"), sloc_end, sloc_end
                    )
                )
            ann = generate_annotations(
                ann_list, subdir=f"ann-{ctc.sublabel}-{index}"
            )
            xcov(
                xcov_args
                + [
                    f"--save-checkpoint={checkpoints[-1]}",
                    f"--external-annotations={ann}",
                ],
            )

        # Try to consolidate them
        output_dir = f"xcov-{ctc.sublabel}"
        mkdir(output_dir)
        log_file = f"cov-{ctc.sublabel}.txt"
        xcov(
            [
                "coverage",
                "--level=stmt",
                "--annotate=xcov",
                f"--output-dir={output_dir}",
            ]
            + [f"--checkpoint={c}" for c in checkpoints],
            out=log_file,
            tolerate_messages=".*",
        )
        thistest.fail_if_not_equal(
            '"gnatcov coverage" output',
            "\n".join(ctc.warnings),
            contents_of(log_file).strip(),
        )
        check_xcov_reports(
            output_dir, {"exempt_overlap.adb.xcov": ctc.expected_cov}
        )

thistest.result()
