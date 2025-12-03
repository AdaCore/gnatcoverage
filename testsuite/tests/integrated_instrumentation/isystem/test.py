"""
Regression test: checks that gnatcov accounts for the isystem switch. It used
to plainly ignore it, which resulted in preprocessing failures.
"""

from SUITE.cutils import Wdir
from SCOV.minicheck import check_xcov_reports
from SUITE.integrated_instr_utils import (
    build_run_and_coverage,
    CompileSource,
    LinkMain,
)
from SUITE.tutils import thistest

Wdir("tmp_")

comp_wf = CompileSource(
    compiler_switches=["-isystem", "../include"], source="../test.c"
)
link_wf = LinkMain(objects=["test.o"])
build_run_and_coverage(wfs=[comp_wf, link_wf], files_of_interest=["../test.c"])

check_xcov_reports(".", {"test.c.xcov": {"+": {6, 7}}})

thistest.result()
