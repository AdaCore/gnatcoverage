"""
Check that gnatcov accepts directory arguments for the --trace and
--sid switches.
"""

import os
import os.path

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov

Wdir("tmp_")

os.mkdir("traces-dir")
env = dict(os.environ)
env.update({"GNATCOV_TRACE_FILE": "traces-dir/"})
build_and_run(
    gprsw=GPRswitches(gprfor(srcdirs=[".."], mains=["test_1", "test_2"])),
    covlevel="stmt",
    mains=["test_1", "test_2"],
    extra_coverage_args=[],
    trace_mode="src",
    program_env=env,
)
xcov(
    [
        "coverage",
        "--level=stmt",
        "--annotate=xcov",
        "--sid",
        "obj/",
        "--trace",
        "traces-dir/",
    ]
)

# Check that the coverage report is as expected
check_xcov_reports(
    ".",
    {
        "test_1.adb.xcov": {"+": {3}},
        "test_2.adb.xcov": {"+": {3}},
    },
)

thistest.result()
