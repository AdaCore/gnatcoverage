"""
Check that gnatcov coverage uses the root project file's object dir to store
reports by default. Check that this can be overriden with --output-dir.
"""

import os.path

from e3.fs import rm

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


wd = Wdir(subdir="tmp_")

# Setup a project file with an explicit object directory specified, build it
# and execute the main.
xcov_args = build_and_run(
    gprsw=GPRswitches(
        root_project=gprfor(
            mains=["pgm.adb"], srcdirs=[".."], objdir="obj", exedir="obj"
        )
    ),
    gpr_obj_dir="obj",
    covlevel="stmt",
    mains=["pgm"],
    gpr_exe_dir="obj",
    extra_coverage_args=["--annotate=xcov"],
)

# Analyze, letting gnatcov pick a place for the report. Check that it picks the
# project's object dir:
rm("obj/pgm.adb.xcov")
xcov(xcov_args, out="coverage1.log")
thistest.fail_if(
    not os.path.exists("obj/pgm.adb.xcov"),
    "could not find expected report in obj dir",
)

# Analyze again, requesting a specific place for the report. Check that it is
# created there:
rm("pgm.adb.xcov")
xcov(xcov_args + ["--output-dir=."], out="coverage2.log")
thistest.fail_if(
    not os.path.exists("pgm.adb.xcov"),
    "could not find expected report in current dir",
)

thistest.result()
