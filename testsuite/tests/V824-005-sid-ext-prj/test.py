"""
Test that SID files are created in the object directory of the extending
project (i.e. not in the extended project's object directory).
"""

import os.path

from e3.fs import mkdir, sync_tree

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir
from SUITE.tutils import xcov


wd = Wdir("tmp_")

# Copy the projec tree in the temporary directory
for filename in ["src-pkg", "src-ext_pkg", "pkg.gpr", "ext_pkg.gpr"]:
    sync_tree(os.path.join("..", filename), filename)

# Go through the instrument/build/run cycle
xcov_args = build_and_run(
    gprsw=GPRswitches(root_project="ext_pkg.gpr", units=["pkg"]),
    covlevel="stmt",
    mains=["main"],
    gpr_obj_dir="obj-ext_pkg",
    extra_coverage_args=["--annotate=xcov", "--output-dir=output"],
    trace_mode="src",
)

# Make sure that SID files were created in the extending project tree, not in
# the extended project tree.
orig_sid = os.path.join("obj-pkg", "pkg.sid")
ext_sid = os.path.join("obj-ext_pkg", "pkg.sid")
thistest.fail_if(os.path.exists(orig_sid), f"Spurious {orig_sid} found")
thistest.fail_if(not os.path.exists(ext_sid), f"Missing expected {ext_sid}")

# Before running "gnatcov coverage", create a dummy SID file in the extended
# project. "gnatcov coverage" should not try to use it, as there is a SID file
# in the extending project.
#
# Note that gprbuild may not create the object directory for the extended
# project, so do it manually.
mkdir("obj-pkg")
with open(orig_sid, "w") as f:
    f.write("dummy SID file\n")

# Compute the coverage report and check its contents (sanity check)
xcov(xcov_args, out="coverage.log")
check_xcov_reports(
    "output",
    {"pkg.adb.xcov": {"+": {11}}, "pkg.ads.xcov": {}},
    discard_empty=False,
)

thistest.result()
