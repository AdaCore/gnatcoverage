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

# Also generate a checkpoint
xcov(xcov_args + ["--save-checkpoint=c.ckpt"])


def check_output(output_file: str, expected_content: str) -> None:
    """
    Check that the content of the "output_file" text file matches
    "expected_content".
    """
    thistest.fail_if_not_equal(
        '"gnatcov coverage" output ({})'.format(output_file),
        expected_content,
        contents_of(output_file).strip(),
    )


def run_and_check(
    args: list[str],
    output_file: str,
    expected_content: str,
    register_failure: bool,
) -> None:
    """
    Run gnatcov with the given command line arguments "args", latch the output
    in "output_file" and check that this output matches the provided
    "expected_content".
    """
    p = xcov(
        args,
        out=output_file,
        tolerate_messages=".*",
        register_failure=register_failure,
    )
    if not register_failure:
        thistest.fail_if(
            p.status == 0,
            'the call to "gnatcov coverage" was expected to fail, yet it'
            f" succeeded (see {output_file})",
        )
    check_output(output_file, expected_content)


# Check that an error is emitted when using --units without -P
run_and_check(
    ["coverage", "--level=stmt", "--units=main", trace_file],
    "missing_project.txt",
    "gnatcov: --units requires -P",
    register_failure=False,
)

# Check the error message when not attempting to provide any SCOs
run_and_check(
    ["coverage", "--level=stmt", trace_file],
    "missing_scos.txt",
    "gnatcov: Please specify SCOs on the command line, specifying Units in"
    " project or using [--units and -P]|--scos|--sid."
    "\nUsage: gnatcov coverage [OPTIONS] TRACE_FILEs"
    "\nRun 'gnatcov coverage --help' for more information.",
    register_failure=False,
)


# Check the warning when --units is passed with --checkpoint but no trace file
run_and_check(
    [
        "coverage",
        f"-P{p}",
        "--level=stmt",
        "-axcov",
        "--units=main",
        "--checkpoint=c.ckpt",
    ],
    "ckpt_units.txt",
    "warning: Specifying units of interest through --units/--excluded-units"
    " has no effect on checkpoints",
    register_failure=True,
)


# Check the absence of warnings when --units is passed with --checkpoint *and*
# a trace file.
run_and_check(
    [
        "coverage",
        f"-P{p}",
        "--level=stmt",
        "-axcov",
        "--units=main",
        "--checkpoint=c.ckpt",
        trace_file,
    ],
    "ckpt_units.txt",
    "",
    register_failure=True,
)

thistest.result()
