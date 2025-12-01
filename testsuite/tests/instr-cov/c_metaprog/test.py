"""
Checks that gnatcov does not instrument decisions in sources of interest whose
parent statement does not belong to a source of interest. This can happen with
metaprogramming instances in C/C++.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.c"])),
    covlevel="stmt+decision",
    mains=["main"],
    extra_instr_args=["--excluded-source-files=meta_inc.h"],
    extra_coverage_args=["-axcov"],
    trace_mode="src",
)

check_xcov_reports("obj", {"main.c.xcov": {"+": {4, 7}}})

thistest.result()
