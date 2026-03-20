"""
This test checks that we have no warning when instrumenting a project
that redefines some runtime files, here a-numaux.ad[sb]
"""

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

wd = Wdir("tmp_")

gpr = gprfor(mains=["main.adb"], srcdirs=["../src"])

build_and_run(
    gprsw=GPRswitches(
        root_project=gpr,
    ),
    mains=["main"],
    covlevel="stmt",
    extra_gprbuild_args=["-gnatg"],
    extra_coverage_args=[],
)

# Ensure `gnatcov instrument` did not confuse runtime files
# (Do not run this check on bin-traces)
if thistest.options.trace_mode != "bin":
    thistest.fail_if_not_equal(
        "Expected empty instrumentation output, but a warning/error was found",
        "",
        contents_of("instrument.log"),
    )

# Ensure the overridden runtime file was used
# On embedded targets, a source trace file will be dumped after the expected
# output. That's why we use a regex.
thistest.fail_if_no_match(
    "If the right a-numaux.ads is used, it will print 123456",
    r"^ 123456\n(== GNATcoverage source trace file ==.*== End ==\n)?",
    contents_of("main_output.txt"),
)

thistest.result()
