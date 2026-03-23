"""
Verify that gnatcov errors out when requested to process a source
trace advertising a trace format version other than the one it (gnatcov)
knows to handle, and that the error message contains an indication of
the trace format versions involved.
"""

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor, xcov

# The test dir contains a dummy program and a corresponding coverage
# srctrace produced from instrumentation by an old version of gnatcov
# (release 23.2) in a native configuration.

# Try to analyze this srctrace and expect the trace version check to
# fail immediately. The check is so early that the actual contents of
# the stored trace doesn't really matter, in particular the target/RTS
# configuration for which the trace was produced compared to that for
# which this test is being run.

tmp = Wdir("tmp_")

gpr = gprfor(srcdirs=[".."], langs=["Ada"], mains=["p0.adb"])

p = xcov(
    [
        "coverage",
        "--level=stmt",
        "-P{}".format(gpr),
        "../p0-23-2.srctrace",
    ],
    register_failure=False,
)

thistest.fail_if(p.status == 0, "analysis succeeded but was expected to fail")

thistest.fail_if_no_match(
    what="output from gnatcov coverage",
    regexp=(
        r"(?s:.*p0-23-2.srctrace: Trace format v2 is not supported "
        "by this version of gnatcov. Handling v[0-9]+ only.)"
    ),
    actual=p.out,
)

thistest.result()
