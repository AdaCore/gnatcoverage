"""
Check the processing of coverage reports generated from LLVM trace adapter.
This test checks the handling of identical code that is formatted differently.
"""

from SUITE.context import thistest
from SUITE.cutils import Wdir, multi_range
from SUITE.llvmutils import check_llvm_reports

_wd = Wdir("tmp_")

incomplete = {19}  # bar

uncovered = {9}  # foo

covered = multi_range(3, 4, 11, 14, 15, 18, 21, 22, (24, 27))


expected_report = {
    "diff_formatting.rs.xcov": {
        "+": covered,
        "!": incomplete,
        "-": uncovered,
    }
}

check_llvm_reports("coverage.json", expected_report)

# With MCDC

mcdc_incomplete = {
    5,  # foo
    19,  # bar
}

mcdc_uncovered = {6, 7, 9}  # foo

mcdc_covered = multi_range(3, 4, 11, 14, 15, 18, 21, 22, (24, 27))

mcdc_expected_report = {
    "diff_formatting.rs.xcov": {
        "+": mcdc_covered,
        "!": mcdc_incomplete,
        "-": mcdc_uncovered,
    }
}


check_llvm_reports(
    "coverage.json", mcdc_expected_report, cov_level="stmt+mcdc"
)

thistest.result()
