"""
Ensure the manual dump trigger flag works as expected in integrated
instrumentation mode.
"""

from SCOV.minicheck import check_xcov_reports
from SUITE.cutils import Wdir
from SUITE.integrated_instr_utils import (
    build_run_and_coverage,
    CompileSource,
    LinkMain,
)
from SUITE.tutils import thistest

Wdir("tmp_")

comp_foo = CompileSource(source="../pkg.cpp", lang="C++")
comp_main = CompileSource(source="../main.cpp", lang="C++")
link_main = LinkMain(objects=["main.o", "pkg.o"], lang="C++")
build_run_and_coverage(
    wfs=[comp_foo, comp_main, link_main],
    files_of_interest=["../pkg.cpp"],
    extra_setup_args=[
        "--dump-trigger=manual",
        "--manual-dump-files=../pkg.cpp",
    ],
)
check_xcov_reports(".", {"pkg.cpp.xcov": {"-": {18}}})

thistest.result()
