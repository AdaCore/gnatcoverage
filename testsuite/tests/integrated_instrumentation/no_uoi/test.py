"""
Check that gnatcov does not break compilation with the -pedantic switch. The
code generated for an empty buffers list used to be uncompilable with
-pedantic. Note that -pedantic should only diagnose warnings and not errors,
so this is not expected, but we can work around the issue easily so fix the
wrong code.
"""

from SUITE.cutils import Wdir
from SUITE.integrated_instr_utils import (
    build_run_and_coverage,
    CompileSource,
    LinkMain,
)
from SUITE.tutils import thistest

Wdir("tmp_")

compiler_switches = ["-pedantic"]
comp_pkg = CompileSource(
    source="../pkg.c", compiler_switches=compiler_switches
)
comp_test = CompileSource(
    source="../test.c", compiler_switches=compiler_switches
)
link_test = LinkMain(objects=["pkg.o", "test.o"])
build_run_and_coverage(
    wfs=[comp_pkg, comp_test, link_test], files_of_interest=["../pkg.c"]
)

thistest.result()
