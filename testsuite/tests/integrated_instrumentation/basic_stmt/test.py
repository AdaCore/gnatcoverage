"""
Basic test for the integrated instrumentation approach. This test checks that
we integrate smoothly in a standard (and very easy) Makefile build process,
that compiles the sources of the project, and then links.

The integrated instrumentation process is as followed:
  * The user sets up the instrumentation runtime
  * The user sets up the instrumentation using the instrument-setup command. He
    must indicate the list of sources of interest (through the --files switch),
    the install location of the instrumentation runtime (through the
    --runtime-install-dir) switch, and the list of compiler wrappers that he
    wants gnatcov to generate (through the --compilers switch).
  * This setup command produces compiler wrappers _and_ an instrumentation
    configuration file. The user must then set his PATH to have the compiler
    wrapper appear _before_ the actual compiler.
  * Then, he can launch the build process unchanged. It will call the compiler
    wrapper instead of the actual compiler, which will instrument on the fly.
  * Running the executable should then produce a trace prefixed with the main
    simple name.
"""

import os.path

from e3.fs import cp

from SUITE.cutils import Wdir
from SCOV.minicheck import check_xcov_reports
from SUITE.integrated_instr_utils import (
    build_run_and_coverage,
    MakefileMain,
)
from SUITE.tutils import thistest

Wdir("tmp_")

# Copy the sources in the temporary directory
cp(os.path.join("..", "main.c"), ".")
cp(os.path.join("..", "pkg.c"), ".")

wfs = [MakefileMain(build_target_deps=["main.o", "pkg.o"])]
build_run_and_coverage(wfs=wfs, files_of_interest=["pkg.c"])

check_xcov_reports(".", {"pkg.c.xcov": {"+": {4}}})

thistest.result()
