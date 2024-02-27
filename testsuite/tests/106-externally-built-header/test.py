"""
Check that GNATcoverage passes the right source directories to the
preprocessing command, including externally built projects source directories.

It used to skip externally built projects when the --externally-built-project
switch was not passed to the gnatcov command line.
"""

import os.path
import re
import shutil

from e3.fs import cp, rm

from SCOV.instr import xcov_instrument
from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir, contents_of, indent
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, gprbuild, gprinstall, xcov

tmp = Wdir("tmp_")

# Create the installation directory and add it to gprbuild's project lookup
# path.
install_dir = os.path.abspath("install")
gpr_install_dir = os.path.join(install_dir, "share", "gpr")
os.mkdir(install_dir)
env.add_search_path("GPR_PROJECT_PATH", gpr_install_dir)

# Build and install the library project
cp(os.path.join("..", "mylib"), ".", recursive=True)
mylib_gpr = os.path.join("mylib", "mylib.gpr")
gprbuild(mylib_gpr)
gprinstall(mylib_gpr, f"--prefix={install_dir}")

# Build the main project using this and run it to produce a trace file
main_gpr = gprfor(
    mains=["main.c"], srcdirs=os.path.join("..", "main"), deps=["mylib"]
)
xcov_args = build_run_and_coverage(
    gprsw=GPRswitches(root_project=main_gpr),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=."],
)

check_xcov_reports(".", {"main.c.xcov": {"+": {6, 7}}})

thistest.result()
