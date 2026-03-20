"""
Check that --annotate=html produces a dynamic html report.
"""

import os

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(mains=["foo.adb"], srcdirs=[".."])),
    covlevel="stmt",
    mains=["foo"],
    extra_coverage_args=["--annotate=html", "--output-dir=html"],
)

js_file = os.path.join("html", "foo.adb.hunk.js")
thistest.fail_if(
    not os.path.exists(js_file), "expecting a dynamic html report"
)

thistest.result()
