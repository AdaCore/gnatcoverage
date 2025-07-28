"""
Check the processing of coverage reports generated from LLVM trace adapter.
This test checks the handling of MCDC instrumentation of nested decisions.

Note: For now, there is a construct (see mcdc_nested.rs) that make the trace\
adapter crash when trying to build the mappings' representation from the
profdata file.
This is an LLVM issue, see https://github.com/llvm/llvm-project/pull/91600
"""

from SUITE.context import thistest
from SUITE.cutils import Wdir, multi_range
from SUITE.llvmutils import check_llvm_reports

_wd = Wdir("tmp_")

uncovered_mcdc_decisions = multi_range(12, 20, (28, 32))  # nested_3

expected_report = {
    "mcdc_nested.rs.xcov": {
        "+": multi_range(
            (3, 9),  # foo
            (11, 17),  # foo_uncovered_nested
            (19, 25),  # foo_uncovered_root
            (27, 38),  # nested_3
            (51, 75),  # main
        )
        - uncovered_mcdc_decisions,
        "!": uncovered_mcdc_decisions,
    }
}

check_llvm_reports("coverage.json", expected_report, cov_level="stmt+mcdc")

thistest.result()
