"""
Check that every file is of interest by default when using integrated
instrumentation without the `--files` switch of the gnatcov setup-integration
command.
"""

import os.path

from e3.fs import cp

from SUITE.cutils import Wdir
from SCOV.minicheck import check_xcov_reports
from SUITE.integrated_instr_utils import build_run_and_coverage, MakefileMain
from SUITE.tutils import thistest

Wdir("tmp_")

# Copy the sources in the temporary directory
cp(os.path.join("..", "main.c"), ".")
cp(os.path.join("..", "pkg.c"), ".")

build_run_and_coverage(
    wfs=[MakefileMain(build_target_deps=["pkg.o", "main.o"])]
)
check_xcov_reports(
    ".", {"pkg.c.xcov": {"+": {4}}, "main.c.xcov": {"+": {6, 7}}}
)

thistest.result()
