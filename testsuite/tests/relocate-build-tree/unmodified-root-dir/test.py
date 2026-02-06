"""
Test the behaviour of the --relocate-build-tree option.
"""

import os

from e3.fs import sync_tree

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


expected_reports = {
    "ops.adb.xcov": {"+": {4, 5}, "-": {6}},
    "ops.ads.xcov": {},
}

# To avoid source repository pollution, copy source material to a temporary
# directory.
Wdir("tmp_")
for filename in ["opslib", "src"]:
    sync_tree(os.path.join("..", filename), filename)

# Create ops project file
extra = """
    for Library_Dir use "lib-opslib";
    for Library_Name use "opslib";
"""
ops_gpr = gprfor([], prjid="ops", extra=extra, cwd="opslib")

# Create tests project file
tests_gpr = os.path.abspath(
    gprfor(
        ["test_inc.adb"],
        prjid="tests",
        deps=["opslib/ops.gpr"],
    )
)

# Build directory is relocated in tmp
build_run_and_coverage(
    gprsw=GPRswitches(
        tests_gpr, units=["ops"], relocate_build_tree=GPRswitches.no_arg
    ),
    covlevel="stmt",
    mains=["test_inc"],
    extra_coverage_args=["--annotate=xcov", "--output-dir=out-instr"],
)

check_xcov_reports("out-instr", expected_reports, discard_empty=False)

# Check if relative path for GPR works correctly
os.environ["GPR_PROJECT_PATH"] += os.pathsep + os.path.dirname(tests_gpr)
build_run_and_coverage(
    gprsw=GPRswitches(
        tests_gpr, units=["ops"], relocate_build_tree=GPRswitches.no_arg
    ),
    covlevel="stmt",
    mains=["test_inc"],
    extra_coverage_args=["--annotate=xcov", "--output-dir=out-instr"],
)

check_xcov_reports("out-instr", expected_reports, discard_empty=False)

thistest.result()
