"""
Test that checkpoints consolidation does not crash in the presence of "null"
SCOs, i.e. duplicated SCOs removed from the SCO vector in
Process_Low_Level_SCOs.
"""

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


wd = Wdir("tmp_")

# Build the test program and generate a trace file + checkpoint
ckpt = "c.ckpt"
build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(["main.c"], srcdirs="..")),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--save-checkpoint", ckpt],
)

# Now try to generate a report from this checkpoint
xcov(["coverage", "-a", "xcov", "-c", "stmt", "--checkpoint", ckpt])

thistest.result()
