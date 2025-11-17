"""
Check the integrated instrumentation on a C++ source.
"""

import os
import os.path

from e3.fs import cp

from SUITE.cutils import Wdir
from SCOV.minicheck import check_xcov_reports
from SUITE.integrated_instr_utils import (
    build_run_and_coverage,
    CompileSource,
    LinkMain,
)
from SUITE.tutils import thistest

Wdir("tmp_")

# Copy the sources in the temporary directory
cp(os.path.join("..", "main.cpp"), ".")

comp_wf = CompileSource(source="main.cpp", lang="C++")
link_wf = LinkMain(objects=[comp_wf.out], lang="C++")
build_run_and_coverage(wfs=[comp_wf, link_wf])
check_xcov_reports(".", {"main.cpp.xcov": {"+": {4}}})

thistest.result()
