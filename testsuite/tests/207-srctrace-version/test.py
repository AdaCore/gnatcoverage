"""
Test that when loading a trace with gnatcov, either everything works correctly
or there is a specific error message concerning the trace version number that
is emitted. If a change in format is detected between the trace and the current
gnatcov version, force the user to update the trace.
"""

import os
import os.path

from SCOV.minicheck import xcov, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

# Generate a project, it will be helpful for test updates
prj = gprfor(mains=["main.adb"], srcdirs=[".."])

# Create a coverage report from the trace under version control.
# Its format is either the same as the one expected by the current gnatcov, in
# which case there should be no problems, or the formats differ, in which case
# we only expect an error message concerning the trace version number.
coverage_log = "coverage_log.txt"
p = xcov(
    [
        "coverage",
        "--output-dir=.",
        "-axcov",
        "-cstmt+mcdc",
        "--sid=../main.sid",
        "--sid=../foo.c.sid",
        "--source-search=..",
        "-T=../reference.srctrace",
    ],
    register_failure=False,
    out=coverage_log
)

# If there was an error, the trace format changed between when the trace was
# generated and now. Look for the appropriate error message.
if p.status != 0:
    thistest.fail_if_no_match(
        "Unexpected error message from 'gnatcov coverage'.\n"
        "*** Ensure the trace format version was properly bumped if you made a"
        " breaking change in the trace format ***\n",
        regexp=r".*gnatcov(\.exe)?: ../reference.srctrace:"
               r" unsupported format version",
        actual=contents_of(coverage_log)
    )

    # If the correct message was found then great, the trace format number was
    # indeed bumped. Now prompt for the test to be updated otherwise we could
    # end up with an old trace in the test, and stop testing with the latest
    # trace format.
    thistest.failed(
        "Update the trace and SIDs in this test to the latest"
        " format to make test pass again.\n"
        "This is simply a matter of running './update.sh' in the test directory."
    )

# Otherwise, check the coverage report
else:
    check_xcov_reports(
        "*.xcov",
        {
            "main.adb.xcov": {"+": {15, 16}, "!": {12}},
            "foo.c.xcov": {"!": {6}}
        }
    )

thistest.result()
