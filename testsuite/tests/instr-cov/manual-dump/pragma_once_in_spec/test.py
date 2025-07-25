"""
Regression test: check that gnatcov does not process headers in the context of
the manual dump trigger preprocessing. To do that, put a #pragma once in a
header, and instrument with -Werror.

Also checks that it puts the right external linkage prefix for a C++ file (e.g.
extern "C"), as otherwise the dependency is not resolved at link time as
gnatcov declares the dump/reset procedures as C symbols.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

gpr = gprfor(
    mains=["main.cpp"],
    prjid="main",
    srcdirs="../src",
    compiler_extra='for Default_Switches("C++") use ("-Werror");',
    extra="""
    package Naming is
      for Spec_Suffix ("C++") use ".hpp";
      for Body_Suffix ("C++") use ".cpp";
    end Naming;
    """,
)
instr_warning = (
    r"warning: Manual buffer dump/reset indications were found" r" in.*"
)
build_run_and_coverage(
    gprsw=GPRswitches(gpr),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov"],
    dump_trigger="manual",
    manual_prj_name="main",
    tolerate_instrument_messages=instr_warning,
)

# Check that we got the expected coverage report

check_xcov_reports(
    "obj", {"main.cpp.xcov": {"+": {12, 13, 15, 17}, "-": {21}}}
)

thistest.result()
