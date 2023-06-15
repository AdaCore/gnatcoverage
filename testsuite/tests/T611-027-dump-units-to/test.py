"""
Check that --dump-units-at works as expected.
"""

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor, xcov
from SUITE.gprutils import GPRswitches


tmp = Wdir('tmp_')

# Generate a project, instrument it and run it
p = gprfor(mains=['main.adb'], srcdirs=['..'])
xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=p),
    covlevel='stmt',
    mains=['main'],
    extra_coverage_args=[])
xcov_args_no_trace = xcov_args[:-1]
trace_file = xcov_args[-1]


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


# Check that --dump-units-to is rejected when used in object coverage mode
run_and_check(
    ['coverage', '-cinsn', '-aasm', trace_file, '-P', p, '--dump-units-to=-'],
    'objcov.txt',
    '.*gnatcov.*: --dump-units-to works in source coverage only',
    regexp=True,
    success_expected=False)

# Check that nothing is dumped without --dump-units-to
run_and_check(xcov_args + ['-axcov'], 'xcov-nodump.txt', '')

# Check that when *not* using the "report" format, the list is printed on
# stdout with --dump-units-to=-.
run_and_check(xcov_args + ['-axcov', '--dump-units-to=-'],
              'xcov-stdout1.txt', 'main\npkg\n')
run_and_check(xcov_args + ['-axcov', '--units=main', '--dump-units-to=-'],
              'xcov-stdout2.txt', 'main\n')

# Check that the list is written to the designated file (foo1.txt) and not on
# the standard output with --dump-units-to=foo1.txt.
run_and_check(xcov_args + ['-axcov', '--dump-units-to=foo1.txt'],
              'xcov-foo.txt', '')
check_output('foo1.txt', 'main\npkg\n')

# Check that with --dump-units-to=-, the list is included in the "report"
# annotation format...

REPORT_PATTERN = (
    '(.|\n)*=+'
    '\n== 4\\. UNITS OF INTEREST =='
    '\n=+'
    '\n'
    '\nmain'
    '\npkg'
    '\n'
    '\n\\*\\* END OF REPORT \\*\\*'
    '\n(.|\n)*')

# ... on the standard output...
run_and_check(
    xcov_args + ['-areport', '--dump-units-to=-'], 'report-stdout.txt',
    REPORT_PATTERN, regexp=True)

# ... or in the designated report file
run_and_check(
    xcov_args + ['-areport', '--dump-units-to=-', '--output=report1.txt'],
    'report-stdout.txt', '')
check_output('report1.txt', REPORT_PATTERN, regexp=True)

# Check that even with "-a report", the list is put in the designated file
# (foo2.txt) with --dump-units-to=foo2.txt. Check also that the report does
# *not* contain the list of units of interest.
run_and_check(
    xcov_args + ['-areport', '--dump-units-to=foo2.txt',
                 '--output=report2.txt'],
    'report-redirected.txt', '')
check_output(
    'report2.txt',
    '(.|\n)*=+'
    '\n== 3\\. ANALYSIS SUMMARY =='
    '\n=+'
    '\n'
    '\nNo STMT violation.'
    '\n'
    '\n\\*\\* END OF REPORT \\*\\*'
    '\n(.|\n)*',
    regexp=True)
check_output('foo2.txt', 'main\npkg\n')

# Now check that gnatcov refuses to dump the list of units of interest when
# --scos or --sid is involved.
if thistest.options.trace_mode == 'bin':
    scos_optname = '--scos'
    scos_ext = 'ali'
else:
    scos_optname = '--sid'
    scos_ext = 'sid'
scos_arg = '{}=obj/main.{}'.format(scos_optname, scos_ext)
run_and_check(
    xcov_args + ['-axcov', '--dump-units-to=.', scos_arg],
    'xcov-scos.txt',
    'We will not be able to dump the list of units of interest: {} is'
    ' present\n'
    '.*gnatcov.*: Cannot dump the list of names for units of interest: see'
    ' above.'.format(scos_optname),
    regexp=True,
    success_expected=False)

# Check that the list of units of interest is written when just creating a
# checkpoint and --dump-units-to is passed.
run_and_check(
    xcov_args + ['--units=main', '--dump-units-to=-',
                 '--save-checkpoint=c1.ckpt'],
    'ckpt-1.txt',
    'main\n')
run_and_check(
    xcov_args + ['--units=pkg', '--save-checkpoint=c2.ckpt'],
    'ckpt-2.txt',
    '')

# Now check that it is properly reconstitued during checkpoint consolidation
run_and_check(
    ['coverage', '-cstmt', '-axcov', '--dump-units-to=-',
     '-Cc1.ckpt', '-Cc2.ckpt'],
    'ckpt-cons.txt',
    'main\npkg\n')

# Finally, check that trying to dump units after loading a checkpoint which
# was created using --scos/--sid is rejected.
run_and_check(
    xcov_args + ['--save-checkpoint=c3.ckpt', scos_arg],
    'ckpt-3.txt',
    '')
run_and_check(
    xcov_args_no_trace + ['-axcov', '--dump-units-to=.', '-Cc3.ckpt'],
    'ckpt-cons-invalid.txt',
    'We will not be able to dump the list of units of interest: c3.ckpt does'
    ' not contain the list of units \\(produced with --scos or --sid\\)\n'
    '.*gnatcov.*: Cannot dump the list of names for units of interest: see'
    ' above.',
    regexp=True,
    success_expected=False)

thistest.result()
