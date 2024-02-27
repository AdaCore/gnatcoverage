"""
Check that we don't consider "not" as a decision operation when standalone.
"""

import os
import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches


p_gpr = os.path.abspath("p.gpr")
obj_dir = os.path.abspath("obj")

tmp = Wdir("tmp_")

# Use --all-decisions so that, even though we are using stmt+decision, we can
# check if the "X and then Y" decision is properly instrumented (thanks to its
# "outcome True not exercized" violation).
#
# TODO: use MC/DC instead not to use --all-decisions. This currently creates
# sources that make GNAT crash.
build_run_and_coverage(
    gprsw=GPRswitches(root_project=p_gpr),
    covlevel="stmt+decision",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov", "--all-decisions"],
    gpr_obj_dir=obj_dir,
    gpr_exe_dir=obj_dir,
    trace_mode="src",
)
check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {
            "+": {4, 5, 7, 9, 11, 12, 15},
            "!": {13},
        }
    },
)

thistest.result()
