"""
Check the content of an XML summary for assertion coverage.
"""

import os

from SCOV.minicheck import build_run_and_coverage
from SUITE.cutils import Wdir, FilePathRefiner
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest

wd = Wdir("tmp_")

gpr = gprfor(["main.adb"], srcdirs="..")
xcov_args = build_run_and_coverage(
    gprsw=GPRswitches(root_project=gpr),
    covlevel="stmt+mcdc+atcc",
    mains=["main"],
    extra_coverage_args=["-axml"],
)

thistest.fail_if_diff(
    os.path.join("..", "src-traces-index.xml.expected"),
    os.path.join("obj", "index.xml"),
    output_refiners=[FilePathRefiner()],
)

thistest.result()
