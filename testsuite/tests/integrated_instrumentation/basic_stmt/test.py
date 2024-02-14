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

import os
import os.path

from e3.fs import cp

from SUITE.control import env
from SUITE.cutils import Wdir
from SCOV.minicheck import check_xcov_reports
from SUITE.tutils import cmdrun, srctracename_for, thistest, xcov

Wdir("tmp_")

cwd = os.getcwd()

# Copy the sources and the Makefile in the temporary directory
cp(os.path.join("..", "Makefile"), ".")
cp(os.path.join("..", "test.c"), ".")
cp(os.path.join("..", "pkg.c"), ".")

# Then, setup the instrumentation process
xcov(
    [
        "setup-integration",
        "--level=stmt",
        f"--files={os.path.join(cwd, 'pkg.c')}",
        "--compilers=gcc",
        f"--output-dir={cwd}",
    ]
)

# Shadow the compiler driver with the generated wrapper
env.add_search_path(env_var="PATH", path=cwd)

# Then, run the build process unchanged
cmdrun(["make", "test"], for_pgm=False)

# Run the executable
cmdrun(["test"], for_pgm=False)

# Check coverage expectations
xcov(
    [
        "coverage",
        "--level=stmt",
        "--sid=pkg.c.sid",
        "-axcov",
        srctracename_for("test"),
    ]
)
check_xcov_reports(".", {"pkg.c.xcov": {"+": {4}}})

thistest.result()
