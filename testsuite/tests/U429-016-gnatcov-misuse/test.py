"""
Check error messages upon incorrectly providing SCOs to gnatcov.
"""

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor, xcov
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

# Generate a project, instrument it and run it
p = gprfor(mains=["main.adb"], srcdirs=[".."])

xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=p),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=[],
)

trace_file = xcov_args[-1]


def check_output(output_file, expected_content):
    """
    Check that the content of the "output_file" text file matches
    "expected_content".
    """
    thistest.fail_if_no_match(
        '"gnatcov coverage" output ({})'.format(output_file),
        expected_content,
        contents_of(output_file),
    )


def run_and_check(args, output_file, expected_content):
    """
    Run gnatcov with the given command line arguments "args", latch the output
    in "output_file" and check that this output matches the provided
    "expected_content".
    """
    p = xcov(args, out=output_file, register_failure=False)
    thistest.fail_if(
        p.status == 0,
        'the call to "gnatcov coverage" was expected to fail, yet it'
        " succeeded (see {})".format(output_file),
    )
    check_output(output_file, expected_content)


# Check that an error is emitted when using --units without -P
run_and_check(
    ["coverage", "--level=stmt", "--units=main", trace_file],
    "missing_project.txt",
    ".*gnatcov.*: --units requires -P",
)

# Check the error message when not attempting to provide any SCOs
run_and_check(
    ["coverage", "--level=stmt", trace_file],
    "missing_scos.txt",
    ".*gnatcov.*: Please specify SCOs on the command line, specifying Units "
    "in project or using \\[--units and -P\\]\\|--scos\\|--sid\\..*",
)

thistest.result()
