"""
Check that generics that are instantiated as ghost entites can be correctly
compiled when instrumented with --instrument-ghost.
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
        covlevel="stmt",
        mains=["main"],
        extra_instr_args=params.extra_instr_args,
        extra_coverage_args=["-axcov", "--output-dir=xcov"],
        extra_gprbuild_args=params.extra_gprbuild_gargs,
    )

    check_xcov_reports(
        "xcov",
        {
            "main.adb.xcov": {"+": {11, 15}},
            "gen.adb.xcov": (
                {"+": {4, 9}}
                if params.instrument_ghost
                else {"+": {9}, "-": {4}}
            ),
            "gen.ads.xcov": {},
            "non_ghost_inst.ads.xcov": {},
        },
        discard_empty=False,
    )

    tmp.to_homedir()

thistest.result()
