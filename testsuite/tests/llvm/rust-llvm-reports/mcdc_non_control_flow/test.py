"""
Check the processing of coverage reports generated from LLVM trace adapter.
This test checksthe handling of MCDC instrumentation in non-control flow
decisions.
Specifically, it checks that the last operand was successfully instrumented.
"""

from SUITE.context import thistest
from SUITE.cutils import Wdir, multi_range
from SUITE.llvmutils import check_llvm_reports

_wd = Wdir("tmp_")

uncovered_mcdc_decisions = {
    9,  # assign_or
    14,  # assign_3
    19,  # assign_3_bis
    24,  # right_comb_tree
}

expected_report = {
    "mcdc_assign.rs.xcov": {
        "+": multi_range(
            (3, 6),  # assign_and
            (8, 11),  # assign_or
            (13, 16),  # assign_3
            (18, 21),  # assign_3_bis
            (23, 26),  # right_comb_tree
            (28, 30),  # foo
            (32, 34),  # func_call
            (36, 62),  # main
        )
        - uncovered_mcdc_decisions,
        "!": uncovered_mcdc_decisions,
    }
}

check_llvm_reports("coverage.json", expected_report, cov_level="stmt+mcdc")

thistest.result()
