"""
Check that gnatcov generates a sarif report inside a "sarif" folder when
multiple report formats are requested.
"""

import os

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


def check_sarif_file(
    ex_cov_args: list[str], sarif_path: str, wdir: str
) -> None:
    tmp = Wdir(wdir)

    build_run_and_coverage(
        gprsw=GPRswitches(
            root_project=gprfor(mains=["main.adb"], srcdirs=[".."])
        ),
        extra_coverage_args=ex_cov_args,
        covlevel="stmt",
        mains=["main"],
        out="coverage.log",
    )
    thistest.fail_if(
        not os.path.exists(sarif_path),
        "sarif report file not in expected location",
    )

    tmp.to_homedir()


# If SARIF is the only requested format to imply the creation of a file, it
# should be under "obj" directly.
thistest.log("SARIF generation as only format generating a file")
check_sarif_file(
    ["--annotate=report", "--annotate=sarif"],
    "obj/coverage.sarif",
    "tmp_sarif_in_obj_",
)

# Otherwise it should be under a "sarif" folder in "obj"
thistest.log("SARIF generation when several formats in comma-separated list")
check_sarif_file(
    ["--annotate=report,sarif,xcov"],
    "obj/sarif/coverage.sarif",
    "tmp_sarif_in_folder_1_",
)
thistest.log(
    "SARIF generation when several formats using multiple --annotate options"
)
check_sarif_file(
    ["--annotate=sarif", "--annotate=xml"],
    "obj/sarif/coverage.sarif",
    "tmp_sarif_in_folder_2_",
)

thistest.result()
