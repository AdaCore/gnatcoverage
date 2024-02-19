"""
Check the integrated instrumentation on a C++ source.
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
cp(os.path.join("..", "test.cpp"), ".")

# Then, setup the instrumentation process
xcov(
    [
        "setup-integration",
        "--level=stmt",
        f"--files={os.path.join(cwd, 'test.cpp')}",
        "--compilers=g++",
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
        "--sid=test.cpp.sid",
        "-axcov",
        srctracename_for("test"),
    ]
)
check_xcov_reports(".", {"test.cpp.xcov": {"+": {6, 7}}})

thistest.result()
