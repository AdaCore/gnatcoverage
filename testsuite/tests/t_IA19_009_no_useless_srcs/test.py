"""
Check that "gnatcov coverage" does not produce reports for units that are not
of interest.
"""

import os.path

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(["test_engines.adb"], srcdirs="../src"),
        units=["engines"],
    ),
    covlevel="stmt",
    mains=["test_engines"],
    extra_coverage_args=["--annotate=xcov"],
    scos=["obj/engines"],
)

thistest.fail_if(
    not os.path.exists("engines.adb.xcov")
    or os.path.exists("test_engines.adb.xcov"),
    "no annotated file when no sco",
)
thistest.result()
