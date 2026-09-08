"""
Check that gnatcov emits the expected warning in case on overlapping "disable
coverage" regions.
"""

from __future__ import annotations

import os.path

from SCOV.minicheck import (
    CovReport,
    build_run_and_coverage,
    check_xcov_reports,
)
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import (
    Cov_Off,
    Cov_On,
    Ext_Annotation,
    generate_annotations,
    gprfor,
)


srcdir = os.getcwd()


def srcfile(filename: str) -> str:
    """
    Helper to compute the absolute path to a source file, regardless of what
    the current working directory is.
    """
    return os.path.join(srcdir, filename)


def check(
    label: str,
    mains: list[str],
    ext_annotations: list[Ext_Annotation],
    warnings: list[str],
    expected_cov: CovReport,
) -> None:
    thistest.log(f"== {label} ==")
    tmp = Wdir(f"tmp_{label}")
    units = [
        os.path.splitext(m)[0] if m.endswith(".adb") else m for m in mains
    ]
    mains = [os.path.splitext(m)[0] for m in mains]
    build_run_and_coverage(
        gprsw=GPRswitches(
            root_project=gprfor(srcdirs=[".."], mains=mains), units=units
        ),
        covlevel="stmt",
        mains=mains,
        extra_instr_args=[
            "--external-annotations",
            generate_annotations(ext_annotations),
        ],
        extra_coverage_args=["--annotate=xcov", "--output-dir=xcov"],
        tolerate_instrument_messages=".*",
    )
    thistest.fail_if_not_equal(
        '"gnatcov instrument" output',
        "\n".join(warnings),
        contents_of("instrument.log").strip(),
    )
    check_xcov_reports("xcov", expected_cov)
    tmp.to_homedir()


check(
    label="basic",
    mains=["cov_off_nested.adb", "lone_cov_on.adb"],
    ext_annotations=[],
    warnings=[
        # All nested regions should be ignored with a warning
        f"*** cov_off_nested.adb:{sloc}: warning: Ignoring annotation"
        " COV_OFF: nested regions are ignored"
        for sloc in ["8:4", "10:7", "16:4"]
    ]
    + [
        # Lone Cov_On annotations (i.e. without a corresponding Cov_Off) should
        # be reported.
        "*** lone_cov_on.adb:4:4: warning: COV_ON annotation found without a"
        " corresponding COV_OFF",
    ],
    expected_cov={
        "cov_off_nested.adb.xcov": {
            "D": set(range(5, 24)) | set(range(25, 28))
        },
        "lone_cov_on.adb.xcov": {"+": {3, 5}},
    },
)

check(
    label="in_ext_independence",
    mains=["lone_cov_on.adb", "lone_cov_off.adb"],
    ext_annotations=[
        Cov_On(srcfile("lone_cov_off.adb"), "5:10", "5:10"),
        Cov_Off(srcfile("lone_cov_on.adb"), "3:4", "3:4", "J"),
    ],
    warnings=[
        # Even though the external annotation puts Cov_On just before the
        # Cov_Off in the source file, external annotations and in-source
        # annotations should be processed separately, so these annotations are
        # not paired, and so Cov_On is discarded, while Cov_Off creates a
        # region that goes until the last SCO in that file.
        "*** lone_cov_off.adb:5:10: warning: COV_ON annotation found without a"
        " corresponding COV_OFF",
        # Likewise, but with the Cov_On in the source code and the Cov_Off in
        # the external annotations.
        "*** lone_cov_on.adb:4:4: warning: COV_ON annotation found without a"
        " corresponding COV_OFF",
    ],
    expected_cov={
        "lone_cov_off.adb.xcov": {"+": {3}, "D": {4, 5, 6}},
        "lone_cov_on.adb.xcov": {"D": {3, 4, 5, 6}},
    },
)

# Check that gnatcov correctly processes sequences of annotations that span
# over multiple files. Currently, this can happen only with C sources: headers
# are instrumented as part of the instrumentation of the bodies, and so we end
# up with annotations sets that mix multiple files.
check(
    label="multi_files",
    mains=["dummy.c"],
    ext_annotations=[
        Cov_Off(srcfile("dummy_1.h"), "7:1", "7:1", "J1E"),
    ],
    warnings=[],
    expected_cov={
        "dummy.c.xcov": {"+": {7, 12}, "D": {8, 9, 10, 11}},
        "dummy_0.h.xcov": {"D": {5, 6, 7}, "+": {4}},
        "dummy_1.h.xcov": {"D": {5, 6, 7, 8}, "+": {4}},
    },
)

# Check various overlapping situations between in-source and external
# annotations.
check(
    label="overlap",
    mains=["cov_off_overlap.adb"],
    ext_annotations=[
        Cov_Off(srcfile("cov_off_overlap.adb"), "3:1", "3:1", "JE1"),
        Cov_On(srcfile("cov_off_overlap.adb"), "7:1", "7:1"),
        Cov_Off(srcfile("cov_off_overlap.adb"), "12:1", "12:1", "JE2"),
        Cov_On(srcfile("cov_off_overlap.adb"), "16:1", "16:1"),
        Cov_Off(srcfile("cov_off_overlap.adb"), "21:5", "21:5", "JE3"),
        Cov_On(srcfile("cov_off_overlap.adb"), "23:1", "23:1"),
        Cov_Off(srcfile("cov_off_overlap.adb"), "30:1", "30:1", "JE3"),
    ],
    warnings=[
        "*** cov_off_overlap.adb:4:4: warning: Ignoring annotation COV_OFF"
        " that intersects with region starting at cov_off_overlap.adb:3:1",
        "*** cov_off_overlap.adb:21:4: warning: Ignoring annotation COV_OFF"
        " that intersects with region starting at cov_off_overlap.adb:21:5",
    ],
    expected_cov={
        "cov_off_overlap.adb.xcov": {
            "+": {8, 18, 24, 26},
            "D": (
                set(range(3, 8))
                | set(range(10, 17))
                | set(range(21, 24))
                | set(range(28, 35))
            ),
        },
    },
)

thistest.result()
