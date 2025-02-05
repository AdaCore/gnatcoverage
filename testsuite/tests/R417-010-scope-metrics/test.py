"""
Check that gnatcov computes correct scope metrics using the XML report format.
produced by gnatcov. Check this for all acceptions: task, entry, subprogram,
package and expression function.
"""

import os

from SCOV.minicheck import build_run_and_coverage
from SUITE.cutils import Wdir, FilePathRefiner
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest

Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(
            srcdirs=[os.path.join("..", "src")], mains=["main.adb"]
        )
    ),
    covlevel="stmt+mcdc",
    mains=["main"],
    extra_coverage_args=["--annotate=xml"],
)

thistest.fail_if_diff(
    os.path.join("..", "main.adb.xml.expected"),
    os.path.join("obj", "main.adb.xml"),
    output_refiners=[FilePathRefiner()],
)
thistest.result()
