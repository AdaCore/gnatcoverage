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

from SUITE.control import env
from SUITE.cutils import Wdir
from SCOV.minicheck import check_xcov_reports
from SUITE.tutils import cmdrun, srctracename_for, thistest, xcov

Wdir("tmp_")

cwd = os.getcwd()

# Copy the sources and the Makefile in the temporary directory
cp(os.path.join("..", "Makefile"), ".")
cp(os.path.join("..", "main.c"), ".")
cp(os.path.join("..", "lib"), ".", recursive=True)

# Then, setup the instrumentation process
xcov(
    [
        "setup-integration",
        "--level=stmt",
        f"--files={os.path.join(cwd, 'lib', 'foo.c')}",
        f"--files={os.path.join(cwd, 'lib', 'bar.c')}",
        "--compilers=gcc",
        f"--output-dir={cwd}",
    ]
)

# Shadow the compiler driver with the generated wrapper
env.add_search_path(env_var="PATH", path=cwd)

# Then, run the build process unchanged
cmdrun(["make"], for_pgm=False)

# Run the executable
cmdrun(["main"], for_pgm=False)

# Check coverage expectations
xcov(
    [
        "coverage",
        "--level=stmt",
        "--sid=foo.c.sid",
        "--sid=bar.c.sid",
        "-axcov",
        srctracename_for("main"),
    ]
)
check_xcov_reports(
    "*.xcov",
    {"bar.c.xcov": {"+": {4}}, "foo.c.xcov": {"+": {4}}},
)

thistest.result()
