"""
Regression test: when using integrated instrumentation and linking against
and instrumented shared library, gnatcov used not to include coverage buffers
from the shared library, which resulted in the shared library units not being
covered.
"""

import os
import os.path

from e3.fs import cp

from SCOV.minicheck import check_xcov_reports
from SUITE.control import env
from SUITE.cutils import contents_of, Wdir
from SUITE.integrated_instr_utils import (
    build_run_and_coverage,
    MakefileMain,
    MakefileSharedLib,
)
from SUITE.tutils import thistest

Wdir("tmp_")

# Copy the sources in the temporary directory
cp(os.path.join("..", "main.c"), ".")
cp(os.path.join("..", "lib"), ".", recursive=True)

# For windows, the OS looks for the DLL in the PATH, and for Linux, it looks
# in the LD_LIBRARY_PATH.
if thistest.env.target.os.name == "windows":
    so_path = "PATH"
    so_name = "libfoobar.dll"
else:
    so_path = "LD_LIBRARY_PATH"
    so_name = "libfoobar.so"

shared_lib_wf = MakefileSharedLib(
    cwd="lib", build_target=so_name, build_target_deps=["foo.o", "bar.o"]
)
main_wf = MakefileMain(
    build_target_deps=["main.o"], linker_switches=["-lfoobar", "-Llib"]
)

# Check that gnatcov warns when building the main that it cannot find the
# shared library dependency (because it relies on ldd, which looks at the
# LD_LIBRARY_PATH to know the shared library location).
build_run_and_coverage(
    wfs=[shared_lib_wf, main_wf],
    files_of_interest=["lib/bar.c", "lib/foo.c"],
    register_failure=False,
)
thistest.fail_if(
    f"warning: Could not find library {so_name}. Add its directory to"
    f" the {so_path} if this is an instrumented library."
    not in contents_of("make.out"),
    "Missing warning in make output",
)
# Then, rerun the whole process with the LD_LIBRARY_PATH set
env.add_search_path(
    so_path,
    os.path.join(os.getcwd(), "lib"),
)
# The shared library has already been built, no need to pass it there. We need
# to clean up the main directory though.
main_wf.clean()
build_run_and_coverage(
    wfs=[main_wf],
    files_of_interest=["lib/bar.c", "lib/foo.c"],
)

check_xcov_reports(".", {"bar.c.xcov": {"+": {4}}, "foo.c.xcov": {"+": {4}}})

thistest.result()
