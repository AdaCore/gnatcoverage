"""
Check that we have the right annotations for SCOs that come from macro
expansion.
"""

import os

from SCOV.minicheck import build_run_and_coverage
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest

Wdir("tmp_")

# For simplicity, avoid commas in these arguments to avoid issues with the
# argument separator for --c-opts.
macro_args = [
    '-DCMDLINE_MACRO_STMT=printf ("Command-line macro");',
    "-DCMDLINE_MACRO_DECL(x)=int x;",
    "-DCMDLINE_MACRO_NO_VALUE(x)x",
]
copts_args = "--c-opts=" + ",".join(macro_args)
build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."],
                                          mains=["test_macros.c"])),
    covlevel="stmt+mcdc",
    mains=["test_macros"],
    extra_instr_args=[copts_args],
    extra_coverage_args=["--annotate=xcov+"]
)

thistest.fail_if_diff(os.path.join("..", "test_macros.c.xcov.expected"),
                      os.path.join("obj", "test_macros.c.xcov"))
thistest.result()
