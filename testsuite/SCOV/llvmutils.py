"""LLVM trace utils functions.

This modules exposes common utility functions specifically for testing the
handling of LLVM Trace Adapter inputs into GNATcov.
"""

from SCOV.minicheck import check_xcov_reports
from SUITE.tutils import xcov


def check_llvm_reports(
    json_file: str,
    expected_report: dict[str, dict[str, set[int]]],
    cov_level: str = "stmt",
    path_prefix: str = "..",
) -> None:
    """
    Check that GNATcov produces the right report when given a JSON file
    generated with the LLVM trace adapter.

    The test case should have this architecture :
    testcase
    +-- src
    |   +-- *.rs
    +-- <json_file>
    +-- test.py
    """
    xcov(
        [
            "coverage",
            f"--level={cov_level}",
            f"--llvm-json-checkpoint=../{json_file}",
            "--source-search=../src",
            "-axcov",
        ]
    )
    check_xcov_reports(".", expected_report)
