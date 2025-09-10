"""
Check that gnatcov complains on missing ALI/SID file.

This runs gnatcov on a simple project that contains two units: main and helper.
Both units are standalone procedures (i.e. mains with no dependency), but only
"main" is declared as a main in the project file, so gprbuild does not compile
helper.

Since helper is not compiled, no ALI/SID file is created for it. However it is
still a unit of interest for gnatcov, so we expect gnatcov to complain about
a missing information file.
"""

import os.path

from e3.fs import rm

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor, xcov


tmp = Wdir("tmp_")

xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=gprfor("main.adb", srcdirs="..")),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov"],
)

# Unlike the compiler, "gnatcov instrument" processes source files in projects,
# even when they are not in the closure of the main. Yet, we want to check that
# gnatcov complains when the SID file is missing, so manually remove it, here.
if thistest.options.trace_mode == "src":
    rm(os.path.join("obj", "helper.sid"))
    what = "SID"
else:
    what = "ALI"

log_file = "coverage.log"
xcov(xcov_args, out=log_file, tolerate_messages=".")
thistest.fail_if_not_equal(
    "gnatcov output",
    "warning: no {} file found for unit helper".format(what),
    contents_of(log_file).strip(),
)

check_xcov_reports("obj", {"main.adb.xcov": {"+": {3}}})

thistest.result()
