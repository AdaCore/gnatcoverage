"""
Test for the integrated instrumentation approach, where we do not split the
compilation process from the link process: both are done through one command.
"""

from SUITE.cutils import Wdir
from SCOV.minicheck import check_xcov_reports
from SUITE.integrated_instr_utils import build_run_and_coverage, LinkMain
from SUITE.tutils import thistest

Wdir("tmp_")
build_run_and_coverage(
    [LinkMain(objects=["../pkg.c", "../main.c"])],
    files_of_interest=["../pkg.c"],
)

# Check the coverage report
check_xcov_reports(".", {"pkg.c.xcov": {"+": {4}}})

thistest.result()
