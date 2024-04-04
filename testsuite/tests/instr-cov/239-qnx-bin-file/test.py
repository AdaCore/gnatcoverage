"""
Check that the "full" profile for GNATcov_RTS works correctly on QNX.
"""

import os.path

from e3.fs import mkdir

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


tmp = Wdir("tmp_")

# Build and install our special gnatcov_rts build
install_dir = os.path.abspath("install")
mkdir(install_dir)
xcov(
    ["setup", "--rts-profile=full", "--prefix", install_dir],
    out="setup.txt",
    force_project_args=True,
    auto_config_args=False,
)

gnatcov_rts_gpr = os.path.join(install_dir, "share", "gpr", "gnatcov_rts.gpr")

# Compute code coverage for the test project, to make sure the runtime's
# implementation of the bin-file dump channel works as expected.
build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov"],
    dump_channel="bin-file",
    runtime_project=gnatcov_rts_gpr,
)
check_xcov_reports("*.xcov", {"main.adb.xcov": {"+": {5}}}, cwd="obj")

thistest.result()
