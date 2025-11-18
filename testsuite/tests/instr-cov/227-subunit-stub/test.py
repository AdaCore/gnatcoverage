"""
Check that computing coverage on an Ada codebase that contains subunits works
correctly when the subunits are stubbed in tests.
"""

import dataclasses
import os.path
from e3.fs import cp, mkdir

from SCOV.instr import xcov_instrument
from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprbuild, xcov


# Note: to avoid "same base name for files" warnings, create local copies of
# project files, but not of sources themselves: project files reference the
# original source directories.


@dataclasses.dataclass
class Test:
    name: str
    """
    Name of this test.

    This is both the name of the project file and the main (executable).
    """

    stubbed_files: list[str]
    """
    List of source files from the "lib" project that are stubbed for this
    testcase.
    """


# Produce one source trace for each testcase. Each testcase use a specific set
# of stubs: exclude them from instrumentation to avoid consolidation warnings
# later on.
cov_argv = []
all_traces = []
for test in [
    Test("test_mult", ["lib-add.adb"]),
    Test("test_exp", ["lib-add.adb", "lib-mult.adb"]),
]:
    thistest.log(f"== {test.name} ==")
    tmp = Wdir(f"tmp_{test.name}")
    cp("../src/*.gpr", ".")

    cov_argv = build_and_run(
        gprsw=GPRswitches(root_project=os.path.abspath(f"{test.name}.gpr")),
        covlevel="stmt+decision",
        mains=[test.name],
        gpr_obj_dir=f"obj/{test.name}",
        gpr_exe_dir="bin",
        extra_instr_args=[
            f"--excluded-source-files={f}" for f in test.stubbed_files
        ],
        extra_coverage_args=[],
    )
    all_traces.append(os.path.abspath(cov_argv.pop()))

    tmp.to_homedir()


# Instrument/compile the "vanilla" (stub-free) library to get the "reference"
# set of SCOs.
thistest.log("== cons ==")
tmp = Wdir("tmp_cons")
cp("../src/*.gpr", ".")
lib_gpr = os.path.abspath("lib.gpr")
mkdir("obj/lib")

if thistest.options.trace_mode == "src":
    xcov_instrument(
        gprsw=GPRswitches(root_project=lib_gpr),
        covlevel="stmt+decision",
        out="instrument.log",
    )
    thistest.fail_if_not_equal(
        '[consolidation] "gnatcov instrument" output',
        "",
        contents_of("instrument.log"),
    )
else:
    gprbuild(lib_gpr)

# Produce a coverage report using test traces and these SCOs
mkdir("xcov")
xcov(
    [
        "coverage",
        "-P",
        lib_gpr,
        "--level=stmt+decision",
        "--annotate=xcov",
        "--output-dir=xcov",
        *all_traces,
    ],
    out="coverage.log",
)
thistest.fail_if_not_equal(
    '[consolidation] "gnatcov coverage" output',
    "",
    contents_of("coverage.log"),
)
check_xcov_reports(
    "xcov",
    {
        "lib.ads.xcov": {},
        "lib.adb.xcov": {},
        "lib-add.adb.xcov": {"-": {3, 4, 6, 7, 8, 10}},
        "lib-mult.adb.xcov": {"+": {5}, "!": {4}, "-": {6, 7, 9}},
        "lib-exp.adb.xcov": {"+": {5}, "!": {4}, "-": {6, 7, 9}},
    },
)

thistest.result()
