"""
Make sure that a base64 trace that has a line which is too long does not
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

    b64_digest = original.split("== GNATcoverage source trace file ==\n")[
        1
    ].split("\n== End ==")[0]

    long_line = "AAAA" * 1000
    digest_with_long_line = f"{b64_digest}\n{long_line}"

    new_b64_trace = (
        "== GNATcoverage source trace file ==\n"
        f"\n{digest_with_long_line}\n"
        "== End =="
    )

    f.write(new_b64_trace)

# Try extracting the source trace from base64, but should fail.
xcov(
    ["extract-base64-trace", INVALID_TRACE_FILE, SHOULD_NOT_EXIST_TRACE],
    register_failure=False,
    err=STDERR_EXTRACT,
)

# Ensure the test failed for the good reason.
thistest.fail_if_no_match(
    "Test is expected to fail because a line is too loong to be "
    "valid base64 trace",
    r".*Unexpected long line in Base64 trace",
    contents_of(STDERR_EXTRACT),
)

# Ensure the command did not generate an invalid srctrace file.
thistest.fail_if(
    exists(SHOULD_NOT_EXIST_TRACE), "shouldn't have created an srctrace file"
)

thistest.result()
