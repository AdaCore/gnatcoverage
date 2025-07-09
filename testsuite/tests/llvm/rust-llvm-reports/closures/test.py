"""
Check the processing of coverage reports generated from LLVM trace adapter.
This test checks the handling of closures' coverage.
"""

from SUITE.context import thistest
from SUITE.cutils import Wdir, multi_range
from SUITE.llvmutils import check_llvm_reports

_wd = Wdir("tmp_")

incomplete = {21}

uncovered = set()

expected_report = {
    "closures.rs.xcov": {
        "+": multi_range((3, 10), (12, 17), (19, 24), (26, 30), minus=[5])
        - incomplete
        - uncovered,
        "!": incomplete,
        "-": uncovered,
    }
}

check_llvm_reports("coverage.json", expected_report)

thistest.result()
