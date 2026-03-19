"""
Test that the consolidation of two checkpoints that have different but
non-overlapping SCOs for the same files due to usage of the Ada preprocessor
with different configurations work as expected.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


for i, param in [
    (1, "True"),
    (2, "False"),
]:
    tmp = Wdir(f"tmp_{i}")

    build_run_and_coverage(
        gprsw=GPRswitches(
            root_project=gprfor(
                srcdirs="..",
                mains=["main.adb"],
                compiler_extra=f"""
                    for Default_Switches ("Ada") use ("-gnateDSAY_HI={param}");
                """,
            )
        ),
        covlevel="stmt",
        mains=["main"],
        extra_coverage_args=["--save-checkpoint=c.ckpt"],
    )

    tmp.to_homedir()

tmp = Wdir("tmp_")
xcov(
    [
        "coverage",
        "--annotate=xcov",
        "--level=stmt",
        "--output-dir=xcov",
        "--checkpoint=../tmp_1/c.ckpt",
        "--checkpoint=../tmp_2/c.ckpt",
    ]
)
check_xcov_reports(
    "xcov",
    {
        "main.adb.xcov": {"+": {5}},
        "pkg.adb.xcov": {"+": {7, 9}},
        "pkg.ads.xcov": {},
    },
)

thistest.result()
