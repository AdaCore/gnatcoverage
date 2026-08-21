"""
Check that we have the right annotations for SCOs that come from
metaprogramming instances (in-function inclusion of headers, combined with
macro expansions), and that the same header can be included from several
functions with different macro definitions in effect.
"""

import os

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

Wdir("tmp_")
build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["test.c"])),
    covlevel="stmt",
    mains=["test"],
    extra_coverage_args=["--annotate=xcov+"],
)

thistest.fail_if_diff(
    os.path.join("..", "test.c.xcov.expected"),
    os.path.join("obj", "test.c.xcov"),
)
thistest.result()
