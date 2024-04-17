"""
Test that when using manual dump trigger in Ada with the appropriate buffers
dump pragma only in a subproject itself dependent on another subproject
gnatcov is able to provide the correct coverage analysis.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


def make_lib_gpr(name, srcdirs, deps):
    return gprfor(
        mains=[],
        prjid=name,
        srcdirs=srcdirs,
        objdir="obj-" + name,
        langs=["Ada"],
        deps=deps,
    )


def check_one(with_units):
    """
    Do a build, run, coverage, and report-check workflow, ensuring we get the
    expected warnings and coverage results.

    with_units controls wether the --units option is passed
    to gnatcov instrument to restrict analysis to the units in the projects
    where the manual dump indication has visibility.
    """

    suffix = "units" if with_units else "no_units"
    tmp = Wdir("tmp_" + suffix)

    make_lib_gpr("lib1", "../src-lib1", ["lib2"])
    make_lib_gpr("lib2", "../src-lib1/src-lib2", None)

    p = gprfor(
        mains=["main.adb"],
        srcdirs=["../src"],
        objdir="obj",
        deps=["lib1", "lib2"],
    )

    # Check that we get the expected coverage reports

    # Running gnatcov natively allows to have one source trace file per
    # project.
    instr_warning = (
        r"warning: Manual buffer dump/reset indications were" r" found in.*"
    )

    build_run_and_coverage(
        gprsw=GPRswitches(
            root_project=p, units=["lib1", "lib2"] if with_units else None
        ),
        covlevel="stmt",
        mains=["main"],
        extra_coverage_args=["-axcov", "--output-dir=xcov_" + suffix],
        trace_mode="src",
        dump_trigger="manual",
        manual_prj_name="lib1",
        tolerate_instrument_messages=instr_warning,
    )

    # If with_units is False, we expect coverage violations for main.adb
    check_xcov_reports(
        "xcov_" + suffix,
        {
            "lib1.adb.xcov": {"+": {6, 8}, "-": {10}},
            "lib1.ads.xcov": {},
            "lib2.adb.xcov": {"+": {4, 6}},
            "lib2.ads.xcov": {},
        }
        | (
            {}
            if with_units
            else {"main.adb.xcov": {"-": {10, 13, 15, 16, 17}}}
        ),
    )
    tmp.to_homedir()


check_one(with_units=False)
check_one(with_units=True)

thistest.result()
