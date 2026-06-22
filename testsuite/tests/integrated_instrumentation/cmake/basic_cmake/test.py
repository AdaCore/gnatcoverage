"""
Regression test: gnatcov used to leave paths passed to the --files switch
un-normalized, which resulted in some units of interest being ignored at
instrumentation time.
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
build_run_and_coverage(wfs=wfs, files_of_interest=["../main.c"])
check_xcov_reports(".", {"main.c.xcov": {"+": {6, 7}}})

thistest.result()
