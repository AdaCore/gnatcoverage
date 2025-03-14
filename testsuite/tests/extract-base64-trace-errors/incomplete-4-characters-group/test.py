"""
Make sure that an invalid base64 trace that is missing characters does not
generate a corrupted srctrace file.
"""

from SCOV.minicheck import build_and_run
from SUITE.cutils import Wdir, contents_of, exists
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest, xcov

Wdir("tmp_")

# First produce a source trace from the test program
xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["--annotate=xcov"],
    trace_mode="src",
    dump_channel="base64-stdout",
)

INVALID_TRACE_FILE = "invalid_trace.b64"
SHOULD_NOT_EXIST_TRACE = "SHOULD_NOT_EXIST.srctrace"
STDERR_EXTRACT = "stderr-extract.txt"

# Then, modify the trace to make it invalid
with open(INVALID_TRACE_FILE, "w") as f:
    original = contents_of("main_output.txt")
    f.write(original.replace("AAAA", "AAA"))

# Try extracting the source trace from base64, but should fail.
xcov(
    ["extract-base64-trace", INVALID_TRACE_FILE, SHOULD_NOT_EXIST_TRACE],
    register_failure=False,
    err=STDERR_EXTRACT,
)

# Ensure the test failed for the good reason.
thistest.fail_if_no_match(
    "Test is expected to fail because we are unable to decode base64 "
    "characters 4 by 4",
    r".*Invalid Base64 trace: incomplete group of 4 characters",
    contents_of(STDERR_EXTRACT),
)

# Ensure the command did not generate an invalid srctrace file.
thistest.fail_if(
    exists(SHOULD_NOT_EXIST_TRACE), "shouldn't have created an srctrace file"
)

thistest.result()
