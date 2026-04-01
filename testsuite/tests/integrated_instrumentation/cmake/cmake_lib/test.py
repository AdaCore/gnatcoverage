"""
Test that gnatcov correctly takes into account instrumented units present
in a static library passed as a positional argument on the link command line.
This is what is done by CMake when defining a library, then using it in an
executable in the same CMakeLists.txt.
"""

from SUITE.cutils import Wdir
from SCOV.minicheck import check_xcov_reports
from SUITE.integrated_instr_utils import (
    build_run_and_coverage,
    CMake,
)
from SUITE.tutils import thistest

Wdir("tmp_")

wfs = [CMake()]
build_run_and_coverage(
    wfs=wfs, files_of_interest=["../src/main.c", "../src/lib.c"]
)
check_xcov_reports(
    ".",
    {
        "main.c.xcov": {"+": {8, 9}},
        "lib.c.xcov": {"+": {6}},
    },
)

thistest.result()
