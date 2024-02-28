import re

from SCOV.minicheck import build_run_and_coverage
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest


tmp_ = Wdir("tmp_")

# We build three programs, each with'ing a package with only no-code items in
# them (p1, p2 and p3). For the first one (p1), only a no-code pragma. For the
# other ones (p2 and p3), a type definition as well.
gpr = gprfor(
    srcdirs=[".."], mains=["test_p1.adb", "test_p2.adb", "test_p3.adb"]
)
# We execute only the second program, then query a coverage report
# for the three packages.
build_run_and_coverage(
    gprsw=GPRswitches(root_project=gpr),
    covlevel="stmt",
    mains=["test_p2"],
    extra_coverage_args=["--annotate=xcov"],
)

# We expect
#
# - no coverage violation on p1. Out of the testing scope, but no SCO of
#   interest to be flagged;
#
# - no coverage violation on p2, with a no-code SCO not to be ignored,
#   within the testing scope;
#
# - a single stmt coverage violation on the type declaration in p3, SCO
#   not to be ignored and unit out of the testing scope.

report_p1 = contents_of("obj/p1.ads.xcov")
thistest.fail_if(
    not re.search(string=report_p1, pattern=r"\.:.*pragma"),
    "pragma not reported as '.' for p1",
)

report_p2 = contents_of("obj/p2.ads.xcov")
thistest.fail_if(
    not re.search(string=report_p2, pattern=r"\.:.*pragma"),
    "pragma not reported as '.' for p2",
)
thistest.fail_if(
    not re.search(string=report_p2, pattern=r"\.:.*type"),
    "type not reported as '.' for p2",
)

report_p3 = contents_of("obj/p3.ads.xcov")
thistest.fail_if(
    not re.search(string=report_p3, pattern=r"\.:.*pragma"),
    "pragma not reported as '.' for p3",
)
if thistest.options.trace_mode == "bin":
    thistest.fail_if(
        not re.search(string=report_p3, pattern=r"\-:.*type"),
        "type not reported as '-' for p3",
    )
else:
    thistest.fail_if(
        not re.search(string=report_p3, pattern=r"\.:.*type"),
        "type not reported as '.' for p3",
    )

thistest.result()
