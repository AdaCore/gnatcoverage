"""
Check the correct loading of JSON files generated with the LLVM trace adapter.
This test checks simple constructions : several decisions in one function,
a non covered case, and a decision not instrumented because of its too
many conditions.
"""

from SUITE.context import thistest
from SUITE.cutils import Wdir, multi_range
from SUITE.llvmutils import check_llvm_reports

_wd = Wdir("tmp_")

incomplete = {36, 48, 50}

uncovered = set()  # foo

covered = (
    multi_range(
        (3, 9),  # bar
        (11, 32),  # several_decisions_in_one_fct
        (34, 42),  # simple_not_covered
        (53, 66),  # condition_too_long
        minus=[14, 22, 23, 30],
    )
    - uncovered
    - incomplete
)

expected_report = {
    "mcdc_basic.rs.xcov": {
        "+": covered,
        "!": incomplete,
        "-": uncovered,
    }
}

check_llvm_reports("coverage.json", expected_report, cov_level="stmt+mcdc")

thistest.result()
