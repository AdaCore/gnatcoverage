"""
Check that every file is of interest by default when using integrated
instrumentation without the `--files` switch of the gnatcov setup-integration
command.
"""

import glob
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

# Check coverage expectations.
xcov(
    [
        "coverage",
        "--level=stmt",
        "-axcov",
        srctracename_for("test"),
    ]
    + [f"--sid={sid}" for sid in glob.glob("*.sid")]
)
check_xcov_reports(
    ".", {"pkg.c.xcov": {"+": {4}}, "test.c.xcov": {"+": {6, 7}}}
)

thistest.result()
