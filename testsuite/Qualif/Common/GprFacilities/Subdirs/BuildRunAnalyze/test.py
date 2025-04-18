"""Test that the --subdirs argument is properly handled."""

import os.path

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


wd = Wdir("tmp_")


gprname = "p"
mainunit = "foo.adb"
subdir = "gnatcov"
mainunit_xcov = os.path.join("obj", subdir, mainunit + ".xcov")

# Arrange to build, run and perform coverage analysis passing
# --subdirs to all gprbuild and gnatcov commands, then verify that
# we find a report in the designated subdir afterwards.

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(prjid=gprname, mains=[mainunit], srcdirs=[".."]),
        subdirs=subdir,
    ),
    covlevel="stmt",
    mains=["foo"],
    extra_coverage_args=["-a", "xcov"],
)

thistest.fail_if(
    not os.path.exists(mainunit_xcov),
    "The coverage report is missing: {}".format(mainunit_xcov),
)

thistest.result()
