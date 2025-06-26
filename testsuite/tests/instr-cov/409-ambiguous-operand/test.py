"""
Check that when the language version permits it, use an if-expr instead of a
Boolean type conversion to avoid the "ambiguous operand" error.

Otherwise, the instrumentation may produce code that is not compilable.
Workarounds include manually qualifying the type to disambigue the expression,
or removing the problematic unit from coverage analysis

NOTE: Assume that the testsuite is run with -gnat05 by default
"""

import re

from SCOV.minicheck import build_and_run
from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.dutils import contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, gprbuild

_wd = Wdir("tmp_")

gpr = gprfor(
    mains=["main_with_pragma.adb"],
    srcdirs=["../src"],
)

# Should work
result = build_and_run(
    gprsw=GPRswitches(root_project=gpr),
    covlevel="stmt",
    mains=["main_with_pragma"],
    register_failure=True,
    extra_coverage_args=[],
)

# Do the same but without the version pragma.
gpr_without = gprfor(
    mains=["main_without_pragma.adb"],
    srcdirs=["../src"],
)
gprsw = GPRswitches(root_project=gpr_without)

xcov_instrument(
    gprsw=gprsw,
    covlevel="stmt",
)
build_p = gprbuild(
    gprsw.root_project, gargs=gprsw.build_switches, register_failure=False
)

thistest.fail_if(build_p.status == 0, "Compilation should have failed")
thistest.fail_if_no_match(
    "Compilation should fail with 'ambiguous operand'",
    re.compile(r".*ambiguous operand.*", flags=re.S),
    contents_of("gprbuild.out"),
)

thistest.result()
