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

# Source files in which to process manual directives
manual_files = [
    "../main.adb",
    "../foo.adb",
    "../foo_c.c",
    "../foo_cpp.cpp",
]


def get_gprsw():
    """
    Generate a project file in the current directory and return the GPRswitches
    instance.
    """
    return GPRswitches(
        gprfor(
            prjid="main",
            srcdirs=[".."],
            mains=["main.adb"],
            langs=["Ada", "C", "C++"],
        ),
    )


#
# Regular cases
#

thistest.log("====== Pass filenames on the command line ======")
tmp = Wdir("tmp_simple")
build_run_and_coverage(
    gprsw=get_gprsw(),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov", "--output-dir=xcov"],
    dump_trigger=",".join(["manual"] + manual_files),
    manual_prj_name="main",
)
check_xcov_reports("xcov", expected_xcov)
tmp.to_homedir()

thistest.log("====== Pass filenames through a response file ======")
tmp = Wdir("tmp_resp")
with open("resp", "w") as f:
    for filename in manual_files:
        print(os.path.abspath(filename), file=f)
build_run_and_coverage(
    gprsw=get_gprsw(),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov", "--output-dir=xcov"],
    dump_trigger="manual,@resp",
    manual_prj_name="main",
)
check_xcov_reports("xcov", expected_xcov)
tmp.to_homedir()

#
# Error cases
#

thistest.log("====== Check error cases ======")
tmp = Wdir("tmp_err")
get_gprsw()

for label, dump_trigger, error in [
    (
        # Check that gnatcov exits with an error when passing a file that does
        # not exist.
        "instr_wrong_file",
        "manual,unknown",
        r".*gnatcov(\.exe)?: File unknown does not exist",
    ),
    (
        # Check that gnatcov exits with an error when passing
        # --dump-trigger=atexit,file.
        "instr_wrong_trigger",
        "atexit,../main.adb",
        r".*gnatcov(\.exe)?: --dump-trigger=atexit|main-end accepts a single"
        " argument",
    ),
]:
    instr_out = f"{label}.out"
    xcov(
        [
            "instrument",
            "-Pmain",
            "--level=stmt",
            f"--dump-trigger={dump_trigger}",
            "--quiet",
        ],
        out=instr_out,
        register_failure=False,
    )
    thistest.fail_if_no_match(
        "missing error in gnatcov instrument output",
        error,
        contents_of(instr_out),
    )

thistest.result()
