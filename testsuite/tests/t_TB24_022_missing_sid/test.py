"""
Check that gnatcov correctly detects missing SID.
"""

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

gprfor(prjid="p1", srcdirs=["../src-p1"], mains=None, langs="Ada")

p2 = gprfor(
    prjid="p2",
    srcdirs=["../src-p2"],
    mains=["p2.adb"],
    deps=["p1"],
    extra='for Origin_Project use "p1";',
)

# Build the projects and produce a trace for the main
build_run_and_coverage(
    gprsw=GPRswitches(root_project=p2),
    covlevel="stmt",
    mains=["p2"],
    extra_coverage_args=[
        "-a",
        "xcov",
        "--no-subprojects",
        "--projects=p2",
        "-v",
    ],
    trace_mode="src",
    tolerate_coverage_messages="no SID file found for unit p2",
)

thistest.fail_if(
    "warning: no SID file found for unit p2"
    not in contents_of("coverage.log"),
    "No warning for missing SID file (for p2)",
)

thistest.result()
