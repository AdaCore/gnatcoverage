"""
Check that GNATcoverage produces a valid output for generics.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

MAIN_BOTH = "main_both.cpp"
MAIN_INT = "main_int.cpp"
MAIN_FLOAT = "main_float.cpp"
MAIN_NONE = "main_none.cpp"


def build_and_run_with_main(main):
    build_run_and_coverage(
        gprsw=GPRswitches(
            root_project=gprfor(
                srcdirs=["../src"],
                mains=[main],
            )
        ),
        covlevel="stmt",
        mains=[main.split(".")[0]],
        extra_instr_args=["--verbose", "--units=pkg.cpp"],
        extra_coverage_args=["-axcov", "--output-dir=xcov", "--units=pkg.cpp"],
        extra_gprbuild_args=["-v"],
    )


# Testcase both instantiation calls
build_and_run_with_main(MAIN_BOTH)
check_xcov_reports(
    "xcov",
    {
        "pkg.cpp.xcov": {
            "+": {
                8,
                14,
                15,
                21,
                22,
            }
        }
    },
)

# Testcase only int instantiation call
build_and_run_with_main(MAIN_INT)
check_xcov_reports(
    "xcov",
    {
        "pkg.cpp.xcov": {
            "-": {
                21,
                22,
            }
        }
    },
)

# Testcase only float instantiation call
build_and_run_with_main(MAIN_FLOAT)
check_xcov_reports(
    "xcov",
    {
        "pkg.cpp.xcov": {
            "-": {
                14,
                15,
            }
        }
    },
)

# Testcase no instantiation call
build_and_run_with_main(MAIN_NONE)
check_xcov_reports(
    "xcov",
    {
        "pkg.cpp.xcov": {
            "-": {
                8,
                14,
                15,
                21,
                22,
            }
        }
    },
)

thistest.result()
