"""
Check that the instrumenter considers the -fno-rtti switch for preprocessing.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprbuild, gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.cpp"])),
    covlevel="stmt",
    mains=["main"],
    extra_instr_args=["--c++-opts=-fno-rtti"],
    extra_gprbuild_cargs=["-fno-rtti"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)

check_xcov_reports(
    "*.xcov",
    {
        "main.cpp.xcov": {"+": {6}},
    },
    "xcov",
)

thistest.result()
