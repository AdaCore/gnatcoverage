"""
Check that "--relocate-build-tree" (no value) is correctly handled when passed
to a subprocess in "gnatcov instrument".
"""

import os

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


wd = Wdir("tmp_")

# Create the test project and a separate build dir for it
p = os.path.abspath(gprfor(["main.adb"], srcdirs=["../src"]))
wd.to_subdir("tmp_/build")

# Run the instrumenter using this build directory. Use -j2 for instrumentation
# to force the use of subcommands in gnatcov.
build_run_and_coverage(
    gprsw=GPRswitches(p),
    covlevel="stmt",
    mains=["main"],
    # --no-subproject is not propagated to "gnatcov instrument-source", so
    # a buggy implementation could pass "--relocate-build-tree main.adb" to it,
    # and thus incorrectly use "main.adb" as the root of the build tree.
    extra_instr_args=[
        "--relocate-build-tree",
        "--no-subprojects",
        "main.adb",
        "-j2",
    ],
    extra_gprbuild_args=["--relocate-build-tree"],
    extra_coverage_args=["--relocate-build-tree", "--annotate=xcov"],
)

check_xcov_reports("obj", {"main.adb.xcov": {"+": {3}}})

thistest.result()
