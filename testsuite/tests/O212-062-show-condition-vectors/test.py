"""
Check that --show-condition-vectors works as expected for MCDC/UC_MCDC and
ATCC.
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
    covlevel='stmt+mcdc+atcc',
    mains=['main'],
    extra_coverage_args=[]
)


def run_and_check(args, output_file, expected_file):
    """
    Run gnatcov with the given arguments.

    :param str output_file: Name of the temporary file to store the content of
        gnatcov's standard output.
    :param str expected_file: File containing text we expected to find in the
        coverage report.
    """
    xcov(args, out=output_file)

    thistest.fail_if_no_match(
        '"gnatcov coverage" output ({})'.format(output_file),
        contents_of(expected_file).replace('\r\n', '\n'),

        # Canonicalize to Unix-style line endings to have cross-platform checks
        contents_of(output_file).replace('\r\n', '\n'))


# For the following test, a different behavior is expected for source and bin
# traces as assertion coverage is currently not supported in binary traces.
# Assertion decisions are expected to be treated as any other decision.

# Check that mcdc/atcc vectors are displayed under the corresponding
# condition violations, and show the conditions indexes in violation messages.

mcdc_atcc_ref_file = ("../bin-mcdc-atcc.expected"
                      if thistest.options.trace_mode == "bin"
                      else "../src-mcdc-atcc.expected")


run_and_check(xcov_args_mcdc + ['-areport', '--show-condition-vectors'],
              'report-stdout-mcdc-atcc.txt',
              mcdc_atcc_ref_file)


# Check that evaluation vectors not part of any pair are displayed below the
# other vectors.

ucmcdc_atcc_ref_file = ("../bin-ucmcdc-atcc.expected"
                        if thistest.options.trace_mode == "bin"
                        else "../src-ucmcdc-atcc.expected")

run_and_check(xcov_args_mcdc + ['--level=stmt+uc_mcdc+atcc',
                                '-areport',
                                '--show-condition-vectors'],
              'report-stdout-ucmcdc-atcc.txt',
              ucmcdc_atcc_ref_file)

thistest.result()
