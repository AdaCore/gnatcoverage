"""
Basic test for the integrated instrumentation approach. This test checks that
we integrate smoothly in a Makefile build process with:
  * A library compiled with its own Makefile in the lib directory, producing
    libfoobar.a and with foo.c and bar.c sources of interest.
  * A main linked with the library (holding instrumented version of the
    sources). The main is not a source of interest itself.
"""

import os
import os.path

from e3.fs import cp

from SUITE.cutils import Wdir
from SUITE.integrated_instr_utils import (
    build_run_and_coverage,
    MakefileMain,
    MakefileStaticLib,
)
from SCOV.minicheck import check_xcov_reports
from SUITE.tutils import thistest

Wdir("tmp_")

cwd = os.getcwd()

# Copy the sources in the temporary directory
cp(os.path.join("..", "main.c"), ".")
cp(os.path.join("..", "lib"), ".", recursive=True)

# Generate the Makefile for the library

wf_lib = MakefileStaticLib(cwd="lib", build_target_deps=["foo.o", "bar.o"])
wf_main = MakefileMain(
    build_target_deps=[os.path.join("lib", wf_lib.build_target), "main.o"],
    linker_switches=["-Llib", "-llib"],
)
build_run_and_coverage(
    wfs=[wf_lib, wf_main], files_of_interest=["lib/foo.c", "lib/bar.c"]
)

check_xcov_reports(".", {"bar.c.xcov": {"+": {4}}, "foo.c.xcov": {"+": {4}}})

thistest.result()
