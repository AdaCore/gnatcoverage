"""
Regression test: gnatcov used to crash when the same project was processed
multiple times.
"""

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")
p1 = gprfor(
    prjid="p1", srcdirs=["../src-p1"], objdir="obj-p1", mains=[], langs=["Ada"]
)
p2 = gprfor(
    prjid="p2",
    srcdirs=["../src-p2"],
    objdir="obj-p2",
    deps=["p1"],
    mains=["p2.adb"],
)

build_run_and_coverage(
    gprsw=GPRswitches(root_project=p2, projects=["p1", "p2"]),
    covlevel="stmt",
    mains=["p2"],
    gpr_obj_dir="obj-p2",
    extra_coverage_args=["-axcov"],
)


thistest.result()
