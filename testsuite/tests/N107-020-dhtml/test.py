"""
Check that --annotate=dthml succeeds and produces something, with a project
file or without.
"""

import os

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


def check(label, scos):
    """
    Check that --annotate=dhtml produces something, using -P or --scos.
    """
    wd = Wdir(subdir=f"tmp_{label}")

    build_run_and_coverage(
        gprsw=GPRswitches(
            root_project=gprfor(mains=["foo.adb"], srcdirs=[".."])
        ),
        covlevel="stmt",
        mains=["foo"],
        extra_coverage_args=["--annotate=dhtml", "--output-dir=dhtml-report"],
        scos=scos,
    )

    index = os.path.join("dhtml-report", "index.html")
    thistest.fail_if(
        not os.path.isfile(index) or os.path.getsize(index) == 0,
        f"missing or empty {index} for {label}",
    )

    wd.to_homedir()


check("scos", scos=["obj/foo"])
check("prj", scos=None)

thistest.result()
