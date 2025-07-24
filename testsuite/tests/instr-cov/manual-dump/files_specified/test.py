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


# Expected xcov report. We have 4 main closures with always the same structure:

#   [main] print 1
#   [main] call unit procedure
#   [main] print 2
#   [main] dump buffers
#
#   [unit] print 3
#   [unit] reset buffers
#   [unit] print 4
#
# We will always process manual annotations in mains (to dump buffers), but the
# annotations in units (to reset buffers) will be ignored in "unit_*_skipped*"
# sources. As a result, the coverage of:
#
#   [main] print 1
#   [main] call unit procedure
#   [unit] print 3
#
# will be 1) covered when unit annotations are ignored and 2) uncovered
# otherwise.
expected_xcov = {
    "main_ada_process.adb.xcov": {"+": {9}, "-": {7, 8}},
    "main_ada_skipped.adb.xcov": {"+": {7, 8, 9}},
    "unit_ada_process.adb.xcov": {"+": {7}, "-": {5}},
    "unit_ada_skipped.adb.xcov": {"+": {5, 7}},
    "main_c_process.adb.xcov": {"+": {9}, "-": {7, 8}},
    "main_c_skipped.adb.xcov": {"+": {7, 8, 9}},
    "unit_c_process.c.xcov": {"+": {8}, "-": {6}},
    "unit_c_skipped.c.xcov": {"+": {6, 8}},
}

# List of source files, as well as the mains in the generated project
source_files = [os.path.splitext(filename)[0] for filename in expected_xcov]
mains_files = [
    filename for filename in source_files if filename.startswith("main_")
]
mains_exe = [os.path.splitext(filename)[0] for filename in mains_files]

# Source files in which to process manual directives
manual_files = [
    os.path.abspath(filename)
    for filename in source_files
    if filename.startswith("main_") or "_skipped" not in filename
]


def get_gprsw():
    """
    Generate a project file in the current directory and return the GPRswitches
    instance.
    """
    return GPRswitches(gprfor(prjid="main", srcdirs=[".."], mains=mains_files))


#
# Regular cases
#

thistest.log("====== Pass filenames on the command line ======")
tmp = Wdir("tmp_simple")
build_run_and_coverage(
    gprsw=get_gprsw(),
    covlevel="stmt",
    mains=mains_exe,
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
    mains=mains_exe,
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
