"""
Check that --show-mcdc-vectors works as expected.
"""

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor, xcov
from SUITE.gprutils import GPRswitches


tmp = Wdir('tmp_')

# Generate a project, instrument it if necessary and run it,
p = gprfor(mains=['main.adb'], srcdirs=['..'])
xcov_args_mcdc = build_and_run(
    gprsw=GPRswitches(root_project=p),
    covlevel='stmt+mcdc',
    mains=['main'],
    extra_coverage_args=[])


def check_output(output_file, expected_content, regexp=False):
    """
    Check that the content of the "output_file" text file matches
    "expected_content". Check for equality if "regexp" is False and consider
    that "expected_content" is a regexp otherwise.

    :param str output_file: Name of the text file whose content needs to be
        checked.
    :param str expected_content: Expected content for this file.
    :param bool regexp: Whether to match as a regexp (by default, check
        content equality).
    """
    checker = (thistest.fail_if_no_match
               if regexp
               else thistest.fail_if_not_equal)
    checker(
        '"gnatcov coverage" output ({})'.format(output_file),
        expected_content,

        # Canonicalize to Unix-style line endings to have cross-platform checks
        contents_of(output_file).replace('\r\n', '\n'))


def run_and_check(args, output_file, expected_content, regexp=False,
                  success_expected=True):
    """
    Run gnatcov with the given arguments.

    :param str output_file: Name of the temporary file to store the content of
        gnatcov's standard output.
    :param str expected_content: See the corresponding argument in "check".
    :param bool regexp: See the corresponding argument in "check".
    :param bool success_expected: If true, the test fails if gnatcov exits with
        an error code. If false, the test fails if gnatcov exits with the zero
        status code.
    """
    p = xcov(args, out=output_file, register_failure=success_expected)
    if not success_expected:
        thistest.fail_if(
            p.status == 0,
            'the call to "gnatcov coverage" was expected to fail, yet it'
            ' succeeded (see {})'.format(output_file)
        )
    check_output(output_file, expected_content, regexp)


# Check that no vector information is dumped without the option
# --show-mcdc-vectors

REPORT_PATTERN_NO_VECTORS = (
    '(.|\n)*'
    '\n'
    r'\n2\.3\. MCDC COVERAGE'
    '\n-+'
    '\n'
    r'\npkg\.adb:6:22: condition has no independent influence pair'
    ', MC/DC not achieved'
    r'\npkg\.adb:6:33: condition has no independent influence pair'
    ', MC/DC not achieved'
    r'\npkg\.adb:20:21: condition has no independent influence pair'
    ', MC/DC not achieved'
    '\n'
    r'\n3 violations\.')

run_and_check(xcov_args_mcdc + ['-areport'], 'report-stdout.txt',
              REPORT_PATTERN_NO_VECTORS, regexp=True)

# Check that mcdc vectors are displayed under the corresponding
# condition violations, and show the conditions indexes in violation messages.

REPORT_PATTERN_MCDC = (
    '(.|\n)*'
    '\n'
    r'\n2\.3\. MCDC COVERAGE'
    '\n-+'
    '\n'
    r'\npkg\.adb:6:22: condition 1 \("B"\) has no independent influence pair'
    ', MC/DC not achieved'
    r'\npkg\.adb:6:33: condition 2 \("C"\) has no independent influence pair'
    ', MC/DC not achieved'
    r'\npkg.adb:6:11: Decision of the form \(\(\(C0 and then C1\)'
    r' and then C2\) or else C3\)'
    '\nEvaluation vectors found:'
    '\n    F - - F  -> FALSE  In a pair for C0, C3'
    '\n    T T F T  -> TRUE  In a pair for C3'
    '\n    T T T -  -> TRUE  In a pair for C0'
    '\n'
    r'\npkg\.adb:20:21: condition 1 \("B"\) has no independent influence pair'
    ', MC/DC not achieved'
    r'\npkg.adb:20:10: Decision of the form \(C0 and then C1\)'
    '\nEvaluation vectors found:'
    '\n    F -  -> FALSE  In a pair for C0'
    '\n    T T  -> TRUE  In a pair for C0'
    '\n'
    '\n'
    r'\n3 violations\.')

run_and_check(xcov_args_mcdc + ['-areport', '--show-mcdc-vectors'],
              'report-stdout.txt', REPORT_PATTERN_MCDC, regexp=True)


# Check that evaluation vectors not part of any pair are displayed
# below the other vectors.

# No need to re-run since we only change the coverage from "stmt+mcdc" to
# "stmt+uc_mcdc".
xcov_args_uc_mcdc = xcov_args_mcdc + ["--level=stmt+uc_mcdc"]

REPORT_PATTERN_UC_MCDC = (
    '(.|\n)*'
    '\n'
    r'\n2\.3\. UC_MCDC COVERAGE'
    '\n-+'
    '\n'
    r'\npkg\.adb:6:22: condition 1 \("B"\) has no independent influence pair'
    ', MC/DC not achieved'
    r'\npkg\.adb:6:33: condition 2 \("C"\) has no independent influence pair'
    ', MC/DC not achieved'
    r'\npkg\.adb:6:44: condition 3 \("A"\) has no independent influence pair'
    ', MC/DC not achieved'
    r'\npkg.adb:6:11: Decision of the form \(\(\(C0 and then C1\)'
    r' and then C2\) or else C3\)'
    '\nEvaluation vectors found:'
    '\n    F - - F  -> FALSE  In a pair for C0'
    '\n    T T T -  -> TRUE  In a pair for C0'
    '\n    T T F T  -> TRUE  Not part of any pair'
    '\n'
    r'\npkg\.adb:20:21: condition 1 \("B"\) has no independent influence pair'
    ', MC/DC not achieved'
    r'\npkg.adb:20:10: Decision of the form \(C0 and then C1\)'
    '\nEvaluation vectors found:'
    '\n    F -  -> FALSE  In a pair for C0'
    '\n    T T  -> TRUE  In a pair for C0'
    '\n'
    '\n'
    r'\n4 violations\.')

run_and_check(xcov_args_uc_mcdc + ['-areport', '--show-mcdc-vectors'],
              'report-stdout.txt', REPORT_PATTERN_UC_MCDC, regexp=True)

thistest.result()
