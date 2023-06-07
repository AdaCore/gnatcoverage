from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

prj = gprfor(srcdirs=[".."], mains=["main1.adb", "main99.adb"])

build_run_and_coverage(gprsw=GPRswitches(root_project=prj),
                       covlevel="stmt",
                       mains=["main99"],
                       extra_coverage_args=["-a", "xcov"])
# For percentages > 99%, GNATcoverage should always round down
thistest.fail_if("99% of 241 lines covered" not in
                 contents_of("obj/pkg.adb.xcov"))

build_run_and_coverage(gprsw=GPRswitches(root_project=prj),
                       covlevel="stmt",
                       mains=["main1"],
                       extra_coverage_args=["-a", "xcov"])
# Symetrically, for percentages < 1%, GNATcoverage should round up
thistest.fail_if("1% of 241 lines covered" not in
                 contents_of("obj/pkg.adb.xcov"))

thistest.result()
