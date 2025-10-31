"""
This test checks that --projects handles correctly the @LISTFILE
"""

import os

from SUITE.tutils import gprfor
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SCOV.minicheck import build_run_and_coverage, check_xcov_reports

tmp = Wdir("tmp_")

a_gpr = gprfor(
    mains=["main.adb"],
    prjid="a",
    srcdirs=os.path.join("..", "a"),
    objdir="obj-a",
    deps=["b"],
)
b_gpr = gprfor(
    mains=[],
    prjid="b",
    srcdirs=os.path.join("..", "b"),
    langs=["Ada"],
    objdir="obj-b",
    deps=["c"],
)
c_gpr = gprfor(
    mains=[],
    prjid="c",
    srcdirs=os.path.join("..", "c"),
    langs=["Ada"],
    objdir="obj-c",
    deps=["d"],
)
d_gpr = gprfor(
    mains=[],
    prjid="d",
    srcdirs=os.path.join("..", "d"),
    langs=["Ada"],
    objdir="obj-d",
)

with open("projects.txt", "w") as f:
    f.write(f"{b_gpr}\n{c_gpr}")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=a_gpr, projects=["@projects.txt"], no_subprojects=True
    ),
    covlevel="stmt",
    mains=["main"],
    gpr_obj_dir="obj-a",
    extra_coverage_args=["-axcov", "--output-dir=."],
)

# Check that we have coverage result only for the project of interest, as
# specified in the gpr file.

check_xcov_reports(
    ".",
    {
        "b.ads.xcov": {},
        "b.adb.xcov": {"+": {6}},
        "c.ads.xcov": {},
        "c.adb.xcov": {"+": {6}},
    },
)

thistest.result()
