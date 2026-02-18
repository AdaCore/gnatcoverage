"""
Test the behaviour of the --relocate-build-tree option.
"""

import os

from e3.fs import sync_tree

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, mkdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


expected_reports = {
    "ops.adb.xcov": {"+": {4, 5}, "-": {6}},
    "ops.ads.xcov": {},
}

# To avoid source repository pollution, copy source material to a temporary
# directory.
cwd = Wdir("tmp_")
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

# Create a separated build dir
build_dir = "./build"
mkdir(build_dir)

# Build directory is relocated in tmp
#
# Use -j2 for instrumentation as it forces the use of subcommands in gnatcov
# for the source instrumentation and the main instrumentation. This in turn
# means that the --relocate-build-tree is propagated to the subcommands, but
# gnatcov generates the command line with spaces between the switch and the
# argument values, so this will exercise both the
# "--relocate-build-switch=value" and the "--relocate-build-tree value" command
# line forms.
build_run_and_coverage(
    gprsw=GPRswitches(tests_gpr, units=["ops"], relocate_build_tree=build_dir),
    covlevel="stmt",
    mains=["test_inc"],
    extra_coverage_args=["--annotate=xcov", "--output-dir=out-instr"],
    extra_instr_args=["-j2"],
)

check_xcov_reports("out-instr", expected_reports, discard_empty=False)

thistest.result()
