"""
Test that compiler output is correctly forwarded to the user on error.
"""

import re

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.integrated_instr_utils import build_run_and_coverage, CompileSource

Wdir("tmp_")

# Setup the instrumentation process
with open("main.c", "w"):
    pass
comp_wf = CompileSource(source="main.c", compiler_switches=["-invalid-switch"])
build_run_and_coverage(wfs=[comp_wf], register_failure=False)

output = contents_of(comp_wf.out_file)
error_msg = "gcc: error: unrecognized command-line option '-invalid-switch'"
thistest.fail_if_no_match(
    "gcc wrapper output", f"(.|\n)*{re.escape(error_msg)}(.|\n)*", output
)
thistest.fail_if_match(
    "gcc wrapper output", re.escape("== gnatcov bug detected =="), output
)

thistest.result()
