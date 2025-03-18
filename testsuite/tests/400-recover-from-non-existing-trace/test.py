"""
This test ensures that when one of the trace files given to `gnatcov coverage`
does not exist, a warning is emitted but the process is not aborted.
"""

import glob
import re

from SCOV.minicheck import build_and_run
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest, xcov

Wdir("tmp_")

# First produce a source trace from the test program
xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."])),
    covlevel="stmt",
    extra_coverage_args=[],
    mains=["main"],
)

LIST_FILE = "traces.txt"
NOT_FOUND_SRCTRACE = "not_found.srctrace"
REAL_SRCTRACE = glob.glob("main-*.srctrace")[0]
COV_OUT = "cov_out.txt"
COV_ERR = "cov_err.txt"

# Make a tracefile list file
with open(LIST_FILE, "w") as f:
    f.write(f"{NOT_FOUND_SRCTRACE}\n{REAL_SRCTRACE}")

# Run the coverage command
xcov(
    ["coverage", "-P", "gen.gpr", "--level=stmt", "-areport", f"@{LIST_FILE}"],
    register_failure=True,
    out=COV_OUT,
    err=COV_ERR,
)

# Ensure a warning was emitted
thistest.fail_if_not_equal(
    "Missing warning to say that a trace file was not found",
    f"warning: {NOT_FOUND_SRCTRACE}: cannot open {NOT_FOUND_SRCTRACE}\n",
    contents_of(COV_ERR),
)

# Ensure the coverage was correctly handled after the encounter with the
# missing file.
thistest.fail_if_no_match(
    "There should be no violation in the report",
    re.compile(r".*No STMT violation\..*", flags=re.S),
    contents_of(COV_OUT),
)

thistest.result()
