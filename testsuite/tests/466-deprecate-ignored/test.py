"""
Checks that gnatcov warns when using the deprecated project command line switch
--ignore-source-files or one of its project attribute equivalent.

Also checks that when using it in addition to the --exclude-source-files
switch, it considers both switches' arguments.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches

wd = Wdir("tmp_")


def run_test_variant(
    excluded_file_args,
    instr_messages,
    cov_messages,
    expected_xcov,
):
    """
    Build, run and generate a coverage report for the project.

    :param list[str] excluded_file_args: List of excluded source file arguments
        to pass to the gnatcov instrument and coverage invocations.
    :param str instr_messages: Regexp for the gnatcov instrument output.
    :param str cov_messages: Regexp for the gnatcov coverage output.
    :param any expected_xcov: Expected coverage results, passed to
        check_xcov_reports.
    """
    build_run_and_coverage(
        gprsw=GPRswitches(root_project="../p.gpr"),
        covlevel="stmt",
        mains=["main"],
        gpr_obj_dir="../obj",
        gpr_exe_dir="../obj",
        extra_instr_args=excluded_file_args,
        extra_coverage_args=excluded_file_args
        + ["-axcov", "--output-dir=xcov"],
        tolerate_instrument_messages=instr_messages,
        tolerate_coverage_messages=cov_messages,
    )

    # Explicitly check that gnatcov instrument/coverage yield deprecation
    # warnings.
    thistest.fail_if_no_match(
        "gnatcov instrument output",
        instr_messages,
        contents_of("instrument.log").strip(),
    )
    thistest.fail_if_no_match(
        "gnatcov coverage output",
        cov_messages,
        contents_of("coverage.log").strip(),
    )
    check_xcov_reports(
        "xcov",
        expected_xcov,
    )


deprecation_prj_warning = (
    "warning: The Ignored_Source_Files.* attribute is deprecated.*"
)
deprecation_switch_warning = (
    "warning: The --ignore-source-files switch is deprecated.*"
)
missing_sid_warning = "warning: no SID file found for unit .*"

# Check by passing the excluded/ignore command line switches, which override
# the project attributes.
run_test_variant(
    excluded_file_args=[
        "--ignore-source-files=p1.adb",
        "--excluded-source-files=p2.adb",
    ],
    instr_messages=deprecation_switch_warning,
    cov_messages="|".join([deprecation_switch_warning, missing_sid_warning]),
    expected_xcov={
        "main.adb.xcov": {"+": {5, 6, 7, 8, 9, 10}},
        "p3.adb.xcov": {"+": {3}},
        "p4.adb.xcov": {"+": {3}},
        "p5.adb.xcov": {"+": {3}},
        "p6.adb.xcov": {"+": {3}},
    },
)

# This time, do not pass any of the excluded/ignore command line switches, to
# check that project attributes are processed as expected.
run_test_variant(
    excluded_file_args=[],
    instr_messages=deprecation_prj_warning,
    cov_messages="|".join([deprecation_prj_warning, missing_sid_warning]),
    expected_xcov={
        "main.adb.xcov": {"+": {5, 6, 7, 8, 9, 10}},
        "p1.adb.xcov": {"+": {3}},
        "p2.adb.xcov": {"+": {3}},
    },
)

thistest.result()
