"""
Check that decisions in Ghost_Predicate aspects are not instrumented.

They used to be instrumented as decisions, and thus be reported as uncovered in
this example program.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SCOV.spark import GhostTestParams
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


for params in GhostTestParams.all_params():
    tmp = Wdir("tmp_" + params.slug)

    build_run_and_coverage(
        gprsw=GPRswitches(
            root_project=gprfor(srcdirs=[".."], mains=["main.adb"])
        ),
        covlevel="stmt+mcdc",
        mains=["main"],
        extra_instr_args=params.extra_instr_args,
        extra_coverage_args=["-axcov", "--output-dir=xcov"],
        extra_gprbuild_args=params.extra_gprbuild_gargs,
    )
    check_xcov_reports(
        "xcov",
        {
            "main.adb.xcov": {"+": {5}},
            "pkg.ads.xcov": {"+": {14}},
        },
    )

    tmp.to_homedir()

thistest.result()
