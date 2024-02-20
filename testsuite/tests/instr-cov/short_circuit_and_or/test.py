"""
Test that when --short-circuit-and-or is passed, or in the presence
of a Short_Circuit_And_Or configuration pragma, gnatcov considers the 'and'
and 'or' boolean operators as having short-circuit semantics, and instruments
their operands as conditions for MC/DC coverage.

This test checks that the Standard.Boolean and/or operators are
considered to have short-circuit semantics (including for subtypes of
Standard.Boolean) but that the and/or operators for a type derived from
Standard.Boolean are not.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


def check_results():
    check_xcov_reports(
        "obj",
        {
            "main.adb.xcov": {"!": {42, 43, 53, 54}, "+": {81, 82, 87, 88}},
            "type_pkg.ads.xcov": {},
        },
    )


tmp = Wdir()

# First, verify that passing --short-circuit-and-or activates the
# instrumentation of non-short circuit boolean operators for MC/DC.
# This part of the test is deactivated for bin traces as the option
# only controls the instrumenter.
if thistest.options.trace_mode == "src":
    tmp.to_subdir("tmp_switch")
    prj = gprfor(
        mains=["main.adb"],
        srcdirs=[".."],
    )

    build_run_and_coverage(
        gprsw=GPRswitches(prj),
        covlevel="stmt+mcdc",
        mains=["main"],
        extra_instr_args=["--short-circuit-and-or"],
        extra_coverage_args=["-axcov"],
    )

    check_results()


# Then, check that the presence of a "pragma Short_Circuit_And_Or" is
# correctly taken into account during instrumentation.
tmp.to_subdir("tmp_config")

prj = gprfor(
    mains=["main.adb"],
    srcdirs=[".."],
    compiler_extra='for Local_Configuration_Pragmas use "../gnat.adc";',
)

build_run_and_coverage(
    gprsw=GPRswitches(prj),
    covlevel="stmt+mcdc",
    mains=["main"],
    extra_coverage_args=["-axcov"],
)

check_results()

thistest.result()
