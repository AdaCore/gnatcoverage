"""
Check the processing of coverage reports generated from LLVM trace adapter.
This test checks the handling of struct impls and trait implementations.
"""

from SUITE.context import thistest
from SUITE.cutils import Wdir, multi_range
from SUITE.llvmutils import check_llvm_reports

_wd = Wdir("tmp_")

incomplete = {}

uncovered = multi_range((32, 34))

expected_report = {
    "impl.rs.xcov": {
        "+": multi_range(
            (8, 12),  # Foo::new
            (14, 16),  # Foo::print
            (24, 26),  # Trait::say_hello
            (28, 30),  # Trait::say_bye
            (38, 40),  # <Foo as Trait>::hello
            (44, 46),  # <Bar as Trait>::hello
            (49, 51),
            53,
            54,
            56,
            58,  # main
        ),
        "!": incomplete,
        "-": uncovered,
    }
}

check_llvm_reports("coverage.json", expected_report)

thistest.result()
