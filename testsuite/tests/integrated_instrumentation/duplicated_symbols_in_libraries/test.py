"""
Check that gnatcov integrated instrumentation only includes coverage buffer
symbols that are in the main link closure. It used to include in the coverage
buffers list every coverage buffer symbol that was in a library / object file
on the link command line. This could result in pulling into the link closure
object files that would have not been included otherwise, and thus result in
clashing symbols.

Also check that gnatcov generates unique SID names when there are homonym
files
"""

import os
import os.path

from e3.fs import cp

from SCOV.minicheck import check_xcov_reports
from SUITE.cutils import Wdir
from SUITE.integrated_instr_utils import (
    build_run_and_coverage,
    MakefileMain,
    MakefileStaticLib,
)
from SUITE.tutils import thistest

Wdir("tmp_")

cwd = os.getcwd()

# Copy the sources in the temporary directory
for item in ["libbar", "libfoo", "main.c"]:
    cp(os.path.join("..", item), ".", recursive=True)
wf_libbar = MakefileStaticLib(
    cwd="libbar", build_target="libbar.a", build_target_deps=["bar.o", "foo.o"]
)
wf_libfoo = MakefileStaticLib(
    cwd="libfoo", build_target="libfoo.a", build_target_deps=["bar.o", "foo.o"]
)
wf_main = MakefileMain(
    build_target_deps=["libbar/libbar.a", "libfoo/libfoo.a", "main.o"],
    linker_switches=["-Llibbar", "-Llibfoo", "-lbar", "-lfoo"],
)
build_run_and_coverage(
    wfs=[wf_libbar, wf_libfoo, wf_main],
    files_of_interest=[
        "libbar/bar.c",
        "libbar/foo.c",
        "libfoo/bar.c",
        "libfoo/foo.c",
    ],
)

check_xcov_reports(
    ".",
    {
        "libbar-bar.c.xcov": {"+": {4}},
        "libbar-foo.c.xcov": {"+": {4}},
        "libfoo-bar.c.xcov": {"-": {4}},
        "libfoo-foo.c.xcov": {"-": {4}},
    },
)

thistest.result()
