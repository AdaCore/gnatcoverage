"""
Check that "gnatcov coverage" is able to process BDD data for the decisions to
cover. This used to crash in source trace mode when a single compilation unit
(in the GCC sense: the unit for pkg.ads, pkg.adb and pkg-compute_in_stub.adb)
contains multiple "CU" (pkg.ads, pkg.adb and pkg-compute_in_stub.adb are 3 CUs
for gnatcov), with multiple CUs containing decisions.
"""

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

p = gprfor(srcdirs=[".."], mains=["main.adb"])
build_run_and_coverage(
    gprsw=GPRswitches(root_project=p),
    covlevel="stmt+mcdc",
    mains=["main"],
    extra_coverage_args=["--save-checkpoint=partial.ckpt"],
)

thistest.result()
