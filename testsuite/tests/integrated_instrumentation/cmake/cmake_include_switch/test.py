"""
Regression test: check that the gcc compiler wrapper correctly discards the
different variations of the include switch when invoking the original compiler
driver command.
"""

import os
import os.path

from SUITE.cutils import Wdir
from SCOV.minicheck import check_xcov_reports
from SUITE.integrated_instr_utils import (
    build_run_and_coverage,
    CMake,
)
from SUITE.tutils import thistest


def process(c_flags: list[str], subdir: str) -> None:
    # Run the integrated instrumentation process with the given c_flags, and in
    # the given subdir
    tmp = Wdir(subdir)
    wfs = [CMake(compiler_switches=c_flags)]
    build_run_and_coverage(
        wfs=wfs, files_of_interest=["../main.c", "../pkg.h"]
    )
    check_xcov_reports(
        ".", {"main.c.xcov": {"+": {5, 6}}, "pkg.h.xcov": {"+": {4}}}
    )
    tmp.to_homedir()


header = os.path.join(os.getcwd(), "pkg.h")
process(["-include", header], "tmp_short_space_arg")
process([f"-include{header}"], "tmp_short_no_space_arg")
process(["--include", header], "tmp_long_space_arg")
process([f"--include={header}"], "tmp_long_eq_arg")

thistest.result()
