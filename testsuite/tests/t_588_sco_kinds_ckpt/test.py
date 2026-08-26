"""
Check the contents of XML reports to verify that the coverage of function,
call and guarded expressions is correctly reported when the coverage
information comes from a checkpoint.
"""

import os

from SCOV.minicheck import build_and_run, xcov
from SUITE.cutils import Wdir, FilePathRefiner
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest

wd = Wdir("tmp_")

xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=gprfor(["main.adb"], srcdirs="..")),
    covlevel="stmt+mcdc+atcc+fun_call+gexpr",
    mains=["main"],
    extra_coverage_args=["-axml"],
    extra_instr_args=["--dump-filename-simple"],
)

xcov(xcov_args + ["--save-checkpoint=c.ckpt"])
# Remove main.srctrace from the arguments list
xcov_args.pop()
xcov(xcov_args + ["--checkpoint=c.ckpt"])

thistest.fail_if_diff(
    os.path.join("..", "main.adb.xml.expected"),
    os.path.join("obj", "main.adb.xml"),
    output_refiners=[FilePathRefiner()],
)

thistest.result()
