"""
Test that we can compute code coverage on Ada code located in a shared library.
Beyond the library, the program is composed of a C unit.
"""

import os

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


wd = Wdir("tmp_")


# Create one project file to build the pure Ada library to test
mylib_gpr = gprfor(
    mains=[],
    prjid="mylib",
    langs=["Ada"],
    srcdirs="../src-mylib",
    objdir="obj-mylib",
    extra='for Library_Name use "mylib";'
    '\nfor Library_Kind use "relocatable";'
    '\nfor Library_Dir use "lib-mylib";',
)

# Create another project to build the mixed C/Ada program to test the library.
main_gpr = gprfor(
    mains=["main.c"],
    prjid="main",
    langs=["C"],
    deps=["mylib"],
    srcdirs="../src-main",
    objdir="obj-main",
)

# Make sure that subprocesses can import the shared library for mylib. On Unix
# systems, there is nothing specific to do thanks to RPATH magic, but on
# Windows we need to add its directory to PATH.
lib_dir = os.path.abspath("lib-mylib")
os.environ["PATH"] = "{}{}{}".format(
    lib_dir, os.path.pathsep, os.environ["PATH"]
)

build_run_and_coverage(
    gprsw=GPRswitches(root_project=main_gpr, projects=["mylib"]),
    covlevel="stmt",
    gpr_obj_dir="obj-main",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov", "--output-dir=."],
    extra_gprbuild_args=["-XLIBRARY_TYPE=relocatable"],
)

check_xcov_reports(
    ".", {"mylib.adb.xcov": {"+": {5, 6}, "-": {8}}, "mylib.ads.xcov": {}}
)

thistest.result()
