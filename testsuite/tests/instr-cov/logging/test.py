"""
Check that "gnatcov instrument" produces the expected debug logging.
"""

import glob
import os.path

from e3.fs import mkdir, sync_tree
from e3.testsuite.driver.diff import Substitute

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import xcov


# Copy projects and their sources in a temporary directory
tmp = Wdir("tmp_")
for pattern in ["*.gpr", "src-*"]:
    for item in glob.glob(os.path.join("..", pattern)):
        sync_tree(item, os.path.basename(item))

# Create an instrumentation output directories to exercise
# INSTRUMENT_CLEAN_OBJDIRS variations.
for project in ["mylib", "harness"]:
    mkdir(os.path.join("obj", project, f"{project}-gnatcov-instr"))

# Run "gnatcov instrument" and check its output against our logging baselines
log = "instr.txt"
xcov(
    [
        "instrument",
        "-Ptests.gpr",
        "-cstmt",
        "--projects=mylib",
        "--log=instrument_clean_objdirs",
    ],
    out=log,
)
thistest.fail_if_diff(
    baseline_file="../baseline.txt",
    actual_file=log,
    failure_message='"gnatcov instrument" output',
    output_refiners=[
        Substitute(os.getcwd(), "[TMP]"),
        Substitute("\\", "/"),
    ],
    ignore_white_chars=False,
)

thistest.result()
