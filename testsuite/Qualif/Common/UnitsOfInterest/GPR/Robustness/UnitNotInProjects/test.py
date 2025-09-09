"""
Check that requesting a unit of interest through --unit for a unit that is
outside of the set of projects selected through --projects is properly
reported.
"""

import os.path
import glob

from e3.fs import mkdir

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")
mkdir("obj-helper")
mkdir("obj-main")

helper_prj = gprfor(
    prjid="helper",
    mains=[],
    langs=["Ada"],
    srcdirs="../src-helper",
    objdir="obj-helper",
)
main_prj = gprfor(
    prjid="main",
    mains=["main.adb"],
    langs=["Ada"],
    deps=["helper"],
    srcdirs="../src-main",
    objdir="obj-main",
)

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=main_prj, projects=["helper"], units=["helper", "main"]
    ),
    covlevel="stmt",
    mains=["main"],
    gpr_obj_dir="obj-main",
    tolerate_instrument_messages="no unit .* in the projects",
    extra_coverage_args=["-axcov"],
    tolerate_coverage_messages="no unit .* in the projects",
)

log_file = (
    "coverage.log"
    if thistest.options.trace_mode == "bin"
    else "instrument.log"
)
thistest.fail_if_not_equal(
    "gnatcov output",
    "warning: no unit main (from --units) in the projects of interest",
    contents_of(log_file).strip(),
)

check_xcov_reports("obj-main", {"helper.adb.xcov": {"+": {3}}})

# Check that all xcov report files are created in obj-main (i.e. the root
# project).
xcov_files = glob.glob("obj-*/*.xcov")
extra_xcov_files = [f for f in xcov_files if os.path.dirname(f) != "obj-main"]
thistest.fail_if_not_equal(
    "misplaced xcov report files",
    "",
    "\n".join(extra_xcov_files),
)

thistest.result()
