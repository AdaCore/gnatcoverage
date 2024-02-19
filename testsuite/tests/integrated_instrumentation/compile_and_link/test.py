"""
Test for the integrated instrumentation approach, where we do not split the
compilation process from the link process: both are done through one command.
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

# Copy the sources in the temporary directory
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

# Then, run the compile + link command
cmdrun(["gcc", "-o", "test", "test.c", "pkg.c"], for_pgm=False)

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
