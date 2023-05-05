"""
Test the behaviour of the --relocate-build-tree and -root-dir option.
"""

import os

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.gprutils import GPRswitches

expected_reports = {
        "ops.adb.xcov": {'+': {4, 5}, '-': {6}},
        "ops.ads.xcov": {},
        }

project_root = os.path.abspath(os.getcwd())+os.sep

# Create ops project file
os.chdir("opslib")
extra="for Library_Dir use \"lib-opslib\";\n\
for Library_Name use \"opslib\";"
ops_gpr = gprfor([], prjid="ops", extra=extra)

# Create tests project file
os.chdir(os.path.join("..", "tests"))
tests_gpr = gprfor(["test_inc.adb"], prjid="tests", deps=["../opslib/ops.gpr"])
abs_test_gpr = os.path.abspath(tests_gpr)

os.chdir("..")

# Build directory is relocated in tmp
wd = Wdir("tmp")
build_run_and_coverage(
    gprsw=GPRswitches(
        abs_test_gpr,
        units=["ops"],
        root_dir=project_root,
        relocate_build_tree=True),
    covlevel="stmt",
    gpr_exe_dir=os.path.join(project_root, "tmp", "tests"),
    mains=["test_inc"],
    extra_coverage_args=["--annotate=xcov", "--output-dir=out-instr"])

check_xcov_reports("*.xcov", expected_reports, "out-instr")

wd.to_homedir()
thistest.result()
