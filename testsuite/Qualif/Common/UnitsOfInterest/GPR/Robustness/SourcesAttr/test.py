"""
Check that gnatcov processes project files with a Source_Files attribute
correctly.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

# We have a sample "flip" unit-to-test and a simple test driver for it, plus a
# additional flop.ads file. We exercize coverage of the flip unit by the
# driver with a project file which explicitly lists the sources of relevance
# to the test, by way of a Source_Files attribute not including flop.ads.

wd = Wdir()


def try_one(subdir, extra_covargs, xreports, xwarnings):
    """
    Setup a temp ``subdir`` and perform a build/run/coverage sequence
    for our example, passing ``extra_covargs`` in addition to gnatcov
    coverage. Verify that we obtain the reports stated as expected
    in ``xreports``, and that possible warnings on units-of-interest
    discrepancies (induced by the extra covargs), stated as expected in
    ``xwarnings``, are found in the logs.
    """

    wd.to_subdir(subdir)
    gpr = gprfor(
        srcdirs="../src",
        mains="test_t.adb",
        extra="\n".join(
            [
                "for Source_Files use",
                '  ("test_t.adb","flip.ads", "flip.adb");',
            ]
        ),
    )

    build_run_and_coverage(
        gprsw=GPRswitches(root_project=gpr),
        covlevel="stmt",
        mains=["test_t"],
        extra_coverage_args=["--annotate=xcov"] + extra_covargs,
        tolerate_coverage_messages=".",
    )

    check_xcov_reports("obj", xreports)

    wlog = contents_of("coverage.log")
    for xw in xwarnings:
        thistest.fail_if(
            xw not in wlog, 'expected warning "%s" not found in log' % xw
        )

    wd.to_homedir()


try_one(
    subdir="wd_1",
    extra_covargs=[],
    xreports={
        "flip.adb.xcov": {"+": {3}},
        "flip.ads.xcov": {},
        "test_t.adb.xcov": {"+": {4, 6}},
    },
    xwarnings=[],
)

try_one(
    subdir="wd_2",
    extra_covargs=["--units=flop"],
    xreports={},
    xwarnings=["no unit flop (from --units) in the projects of interest"],
)

thistest.result()
