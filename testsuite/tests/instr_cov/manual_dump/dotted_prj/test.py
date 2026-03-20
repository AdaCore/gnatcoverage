"""
Test that the sources generated when using --dump-trigger=manual on a project
which has a dotted name compile correctly.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

# Empty dummy project, just to make the Base.Prj a legal project
empty = gprfor(srcdirs=[], prjid="empty", mains=[])

# The prjid is at the heart of this test, keep it a dotted name
gpr = gprfor(
    mains=["main.adb", "main_c.c"],
    prjid="empty.prj",
    srcdirs="../src",
    deps=[empty],
    langs=["Ada", "C"],
)

build_run_and_coverage(
    gprsw=GPRswitches(gpr),
    covlevel="stmt",
    mains=["main", "main_c"],
    extra_coverage_args=["-axcov"],
    dump_trigger="manual",
    manual_prj_name="empty.prj",
)

# Check that we got the expected coverage report
check_xcov_reports(
    "obj",
    {
        "main_c.c.xcov": {"+": {6}, "-": {7}},
        "pkg_c.c.xcov": {},
        "main.adb.xcov": {"+": {5}},
        "pkg.adb.xcov": {},
        "pkg.ads.xcov": {},
    },
)

thistest.result()
