"""
Check that gnatcov setup / instrument support custom toolchain definitions that
are passed through the --db command line option.
"""

import os

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor, xcov
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

prj = gprfor(
    mains=["main.c"],
    srcdirs=[".."],
    extra='for Toolchain_Name ("C") use "MY_GCC";',
)
db_args = ["--db", "../my_config"]

# Setup with our custom toolchain
xcov(
    ["setup", "--prefix=install"] + db_args
    + ["../gnatcov-rt-custom/gnatcov_rts_custom.gpr"],
    register_failure=True,
    auto_config_args=False,
    force_project_args=True,
)
env.add_search_path("GPR_PROJECT_PATH", "install/share/gpr")
build_run_and_coverage(
    gprsw=GPRswitches(prj),
    covlevel="stmt",
    mains=["main"],
    extra_args=db_args,
    extra_gprbuild_args=db_args,
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    extra_instr_args=["--runtime-project", "gnatcov_rts_custom"],
    auto_config_args=False,
)
check_xcov_reports("xcov", {"main.c.xcov": {"+": {4}}})

thistest.result()
