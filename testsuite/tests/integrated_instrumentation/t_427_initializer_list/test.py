"""
Test the integrated instrumentation on an initializer list, to verify that, on
native x86 compiler this doesn't cause parsing errors in libclang and we get
the expected coverage results.
"""

import os
import os.path

from e3.fs import cp

from SCOV.minicheck import check_xcov_reports
from SUITE.cutils import Wdir
from SUITE.integrated_instr_utils import (
    build_run_and_coverage,
    CompileSource,
    LinkMain,
)
from SUITE.tutils import thistest

Wdir("tmp_")

cwd = os.getcwd()

# Copy the sources in the temporary directory
cp(os.path.join("..", "test_for_range.cpp"), ".")

# Run the build process
comp_wf = CompileSource(source="test_for_range.cpp")
link_wf = LinkMain(exec_name="test_for_range", objects=[comp_wf.out])
build_run_and_coverage(wfs=[comp_wf, link_wf])

check_xcov_reports(".", {"test_for_range.cpp.xcov": {"+": {6, 7, 9, 11}}})

thistest.result()
