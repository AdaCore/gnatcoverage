"""
Test that gnatcov does not crash when the source locations passed to the
--start-location or --end-location options, and instead exit in an error.
"""

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import xcov, contents_of

tmp = Wdir("tmp_")


def check_one(arg_name, invalid_line):
    """
    Run gnatcov add-annotation to generate an annotation for main.adb, with
    either a line or a column set to 0 in the value passed to arg_name, and
    check that gnatcov correctly rejects the argument with an expected error
    message.

    arg_name must be one of location, start-location or end-location.
    """
    log_filename = arg_name + ("_line" if invalid_line else "_col") + ".log"

    sloc = "0:1" if invalid_line else "1:0"

    # Pass the requested argument to gnatcov. For start/end location arguments,
    # also add the complementary argument.
    sloc_args = [f"--{arg_name}={sloc}"]
    if arg_name != "location":
        sloc_args += [
            (
                "--end-location"
                if arg_name == "start-location"
                else "--start-location"
            ),
            "1:1",
        ]

    p = xcov(
        [
            "add-annotation",
            "--kind",
            "exempt_on" if arg_name == "location" else "exempt_region",
            "--justification=dummy",
            "--output=dummy.toml",
        ]
        + sloc_args
        + ["../main.adb"],
        out=log_filename,
        register_failure=False,
    )
    thistest.fail_if(p.status == 0, "zero return status not expected")

    expected_err = (
        r".*gnatcov(\.exe)?: "
        + ("Line" if invalid_line else "Column")
        + f" number in argument to --{arg_name} should not be 0"
    )

    thistest.fail_if_no_match(
        what="wrong 'gnatcov add-annotation' error message",
        regexp=expected_err,
        actual=contents_of(log_filename),
    )


for arg_name in ["location", "start-location", "end-location"]:
    for invalid_line in [True, False]:
        check_one(arg_name, invalid_line)

thistest.result()
