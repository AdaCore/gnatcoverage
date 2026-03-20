"""
Test that gnatcov can read source trace files in any endianity.

This test rewrites an existing trace file itself so that we can test both
endianity even though we are running programs on a host that supports a single
endianity.
"""

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.srctracelib import SrcTraceFile
from SUITE.stream_decoder import ByteStreamDecoder
from SUITE.tutils import gprfor, xcov


tmp = Wdir("tmp_")

# Build a project, run it and create a source trace file
xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=gprfor("main.adb", srcdirs="..")),
    covlevel="stmt+mcdc",
    mains=["main"],
    extra_coverage_args=["-axcov"],
    trace_mode="src",
)
trace_file = xcov_args[-1]

# Read this trace file and generate two other files, one per endianity
with open(trace_file, "rb") as f:
    tf = SrcTraceFile.read(ByteStreamDecoder(f))
with open("le.srctrace", "wb") as f:
    tf.endianity = "little-endian"
    tf.write(f)
with open("be.srctrace", "wb") as f:
    tf.endianity = "big-endian"
    tf.write(f)

# Check that gnatcov decodes both the same way
for discr in ("be", "le"):
    xcov_dir = f"xcov-{discr}"
    xcov(
        xcov_args + ["--output-dir", xcov_dir, f"{discr}.srctrace"],
        out=f"coverage-{discr}.log",
    )
    check_xcov_reports(
        xcov_dir,
        {
            "main.adb.xcov": {"+": {5, 7, 8}},
            "pkg.adb.xcov": {"+": {6, 8}, "!": {5}},
            "pkg.ads.xcov": {},
        },
        discard_empty=False,
    )

thistest.result()
