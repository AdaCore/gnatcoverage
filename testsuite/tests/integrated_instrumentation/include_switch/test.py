"""
Regression test: when the user passed the -include switch to gcc, it was
processed both by gnatcov when instrumenting the file and by gcc when compiling
the instrumented file. This resulted in reincluding the file, which could yield
a compilation error. GNATcov now passes the -fpreprocessed switch to the gcc
invocation, which means it now longer tries to preprocess the instrumented file
(that is already preprocessed).
"""

import os.path

from e3.fs import cp

from SUITE.cutils import Wdir
from SCOV.minicheck import check_xcov_reports
from SUITE.integrated_instr_utils import (
    CompileSource,
    build_run_and_coverage,
    LinkMain,
    setup_integration,
)
from SUITE.tutils import thistest

Wdir("tmp_")

# Copy the sources in the temporary directory
cp(os.path.join("..", "test.c"), ".")
cp(os.path.join("..", "glob.h"), ".")

env = setup_integration()
CompileSource(compiler_switches=["-include", "glob.h"], source="test.c").build(
    env=env
)
CompileSource(
    compiler_switches=["--include", "glob.h"], source="test.c"
).build(env=env)
CompileSource(compiler_switches=["--include=glob.h"], source="test.c").build(
    env=env
)
CompileSource(compiler_switches=["--include=glob.h"], source="test.c").build(
    env=env
)
build_run_and_coverage(wfs=[LinkMain(objects=["test.o"])])

check_xcov_reports(".", {"test.c.xcov": {"+": {4}}})

thistest.result()
