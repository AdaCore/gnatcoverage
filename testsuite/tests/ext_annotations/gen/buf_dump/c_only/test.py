"""
Test that when using manual dump trigger in C with a specific comment
indicating where to dump the coverage buffers, gnatcov is able to replace it
with a call to the dump buffers procedure and output correct traces.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import Dump_Buffers, generate_annotations, gprfor
from SUITE.gprutils import GPRswitches

# Generate annotations out of the temp dir to avoid having relative path
# components in the entries

annotations = generate_annotations(
    [
        Dump_Buffers("src/main.c", "17:7", None),
        Dump_Buffers("src/main.c", "24:3", None),
        Dump_Buffers("src-lib/foo.c", "6:3", None),
    ]
)

tmp = Wdir("tmp_")

# Create the GPR file for the root project
src_gpr = gprfor(
    mains=["main.c"],
    prjid="main",
    srcdirs="../src",
    objdir="obj",
    langs=["C"],
    deps=["lib"],
)

# Create the GPR files for the library
lib_gpr = gprfor(
    mains=[],
    prjid="lib",
    srcdirs="../src-lib",
    langs=["C"],
    extra="""
                     for Library_Name use "lib";
                     for Library_Dir use "lib";
                 """,
)

gprsw = GPRswitches(root_project=src_gpr)

instr_warning = (
    r"warning: Manual buffer dump/reset indications were found" r" in.*"
)

build_run_and_coverage(
    gprsw=gprsw,
    covlevel="stmt",
    mains=["main"],
    extra_instr_args=[f"--external-annotations={annotations}"],
    extra_coverage_args=["-axcov"],
    extra_gprbuild_args=["-q"],
    dump_trigger="manual",
    manual_prj_name="main",
    tolerate_instrument_messages=instr_warning,
)

thistest.fail_if_not_equal(
    what="gprbuild output not empty",
    expected="",
    actual=contents_of("gprbuild.out").strip(),
)

# Check that that the dump call indication was correctly replaced in the sub
# project

lib_file = "obj/lib-gnatcov-instr/foo.c"
thistest.fail_if_no_match(
    "missing Dump_Buffers call",
    r"(\n|.)*gnatcov_rts_manual_dump_buffers_lib\(.*\);(\n|.)*",
    contents_of(lib_file),
)

# Check that we got the expected coverage report

check_xcov_reports(
    "obj",
    {
        "main.c.xcov": {"+": {6, 12, 15, 20, 22}, "-": {18, 26}},
        "foo.c.xcov": {"+": {4, 8}},
    },
)

thistest.result()
