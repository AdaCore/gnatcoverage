"""
Test that when using manual dump trigger in Ada with a specific pragma
indicating where to dump the coverage buffers, gnatcov is able to replace it
with a call to the dump buffers procedure and output correct traces.
"""

import glob

from SCOV.minicheck import build_and_run, check_xcov_reports, xcov
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import (
    Dump_Buffers,
    generate_annotations,
    gprfor,
    srctrace_pattern_for,
)

# Generate annotations out of the temp dir to avoid having relative path
# components in the entries

annotations = generate_annotations(
    [
        Dump_Buffers("src/manual_dump.adb", "4:7", None, insert_after=True),
        Dump_Buffers("src-lib1/lib1.adb", "6:9", None, insert_after=True),
        Dump_Buffers("src-lib2/lib2.adb", "6:9", None, insert_after=True),
    ]
)

tmp = Wdir("tmp_")

lib1_p = gprfor(
    mains=[],
    prjid="lib1",
    srcdirs="../src-lib1",
    objdir="obj-lib1",
    langs=["Ada"],
)

lib2_p = gprfor(
    mains=[],
    prjid="lib2",
    srcdirs="../src-lib2",
    objdir="obj-lib2",
    langs=["Ada"],
)

p = gprfor(
    prjid="gen",
    mains=["main.adb"],
    srcdirs=["../src"],
    objdir="obj",
    deps=["lib1", "lib2"],
)

instr_warning = (
    r"warning: Manual buffer dump/reset indications were found" r" in.*"
)

cov_args = build_and_run(
    gprsw=GPRswitches(root_project=p, units=["lib1", "main"]),
    covlevel="stmt",
    mains=["main"],
    extra_instr_args=[f"--external-annotations={annotations}"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
    dump_trigger="manual",
    manual_prj_name="gen",
    tolerate_instrument_messages=instr_warning,
)

# Check that gnatcov inserted the call to the dump buffers procedure in the
# lib2.adb which is not a unit of interest


def check_call(file):
    thistest.fail_if_no_match(
        "missing dump buffers procedure call",
        "(\n|.)*GCVRT.DB_manual_lib2.Dump_Buffers(.*);" "(\n|.)*",
        contents_of(file),
    )


check_call("obj-lib2/lib2-gnatcov-instr/lib2.adb")

# Generate and check coverage reports, using the last trace generated for the
# root project.
trace_file = sorted(
    glob.glob(srctrace_pattern_for("main", manual=True, manual_prj_name="gen"))
)[-1]

xcov(cov_args + [trace_file], out="coverage.log")
check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {"+": {12, 15, 19, 24, 26, 27, 28}, "-": {21, 29}},
        "lib1.adb.xcov": {"+": {4, 7}},
        "lib1.ads.xcov": {},
    },
)

thistest.result()
