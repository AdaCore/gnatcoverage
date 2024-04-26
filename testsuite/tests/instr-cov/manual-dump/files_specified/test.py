"""
This test checks that when instrumenting a file with
`--dump-trigger=manual,@file`, where file contains the list of files with
manual indications, gnatcov only processes the specific files.
"""

import os

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports, xcov
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

test_dir = os.getcwd()

# Expected xcov report
expected_xcov = {
    "main.adb.xcov": {"+": {18, 22, 26}, "-": {20, 24, 28}},
    "foo.adb.xcov": {"+": {5}},
    "foo_skipped.adb.xcov": {"-": {5}},
    "foo_c.c.xcov": {"+": {4}},
    "foo_skipped_c.c.xcov": {"-": {4}},
    "foo_cpp.cpp.xcov": {"+": {4}},
    "foo_skipped_cpp.cpp.xcov": {"-": {4}},
}


def GPRswitches_wrapper():
    """
    Generate a project file in the current directory and return the GPRswitches
    """
    return GPRswitches(
        gprfor(
            prjid="main",
            srcdirs=[".."],
            mains=["main.adb"],
            langs=["Ada", "C", "C++"],
        ),
    )


thistest.log("====== Pass filenames on the command line ======")
tmp = Wdir("tmp_simple")

# Check by passing a simple filename
build_run_and_coverage(
    gprsw=GPRswitches_wrapper(),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov", "--output-dir=xcov"],
    dump_trigger="manual,../main.adb,../foo.adb,../foo_c.c,../foo_cpp.cpp",
    manual_prj_name="main",
)
check_xcov_reports("xcov", expected_xcov)

# Check by passing a response file
thistest.log("====== Pass filenames through a response file ======")
tmp.to_homedir()
tmp = Wdir("tmp_resp")

with open("resp", "w") as resp_file:
    resp_file.write(os.path.join(test_dir, "main.adb") + "\n")
    resp_file.write(os.path.join(test_dir, "foo.adb") + "\n")
    resp_file.write(os.path.join(test_dir, "foo_c.c") + "\n")
    resp_file.write(os.path.join(test_dir, "foo_cpp.cpp") + "\n")
build_run_and_coverage(
    gprsw=GPRswitches_wrapper(),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov", "--output-dir=xcov"],
    dump_trigger="manual,@resp",
    manual_prj_name="main",
)
check_xcov_reports("xcov", expected_xcov)

# Check error cases
thistest.log("====== Check error cases ======")
tmp.to_homedir()
tmp = Wdir("tmp_err")
GPRswitches_wrapper()

# Check that gnatcov exits with an error when passing a file that does not
# exist.
instr_out = "instr_wrong_file.out"
xcov(
    [
        "instrument",
        "-Pmain",
        "--level=stmt",
        "--dump-trigger=manual,unknown",
        "--quiet",
    ],
    out=instr_out,
    register_failure=False,
)
thistest.fail_if_no_match(
    "missing error in gnatcov instrument output",
    r".*gnatcov(\.exe)?: File unknown does not exist",
    contents_of(instr_out),
)

# Check that gnatcov exits with an error when passing
# --dump-trigger=atexit,file.
instr_out = "instr_wrong_trigger.out"
xcov(
    [
        "instrument",
        "-Pmain",
        "--level=stmt",
        "--dump-trigger=atexit,../main.adb",
        "--quiet",
    ],
    out=instr_out,
    register_failure=False,
)
thistest.fail_if_no_match(
    "missing error in gnatcov instrument output",
    r".*gnatcov(\.exe)?: --dump-trigger=atexit|main-end accepts a single"
    " argument",
    contents_of(instr_out),
)

thistest.result()
