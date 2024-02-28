import re

from SCOV.minicheck import build_run_and_coverage
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import thistest, gprfor


tmp_ = Wdir("tmp_")

gpr = gprfor(srcdirs="../src", mains=["ut_main.adb"])
build_run_and_coverage(
    gprsw=GPRswitches(root_project=gpr),
    covlevel="stmt",
    mains=["ut_main"],
    extra_coverage_args=["--annotate=xcov"],
)

# We do not expect uncovered lines in package_a, as the code generated
# for the interface type declaration is thunk code, which is to be
# ignored for coverage purposes.

report_package_a = contents_of("obj/package_a.ads.xcov")
thistest.fail_if(
    re.search(string=report_package_a, pattern=r"-:"),
    "unexpected uncovered code",
)

thistest.result()
