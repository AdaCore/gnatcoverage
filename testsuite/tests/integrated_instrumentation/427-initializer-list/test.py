"""
Test the integrated instrumentation on an initializer list, to verify that, on
native x86 compiler this doesn't cause parsing errors in libclang and we get
the expected coverage results.
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
cp(os.path.join("..", "test_for_range.cpp"), ".")

# Then, setup the instrumentation process
xcov(
    [
        "setup-integration",
        "--level=stmt",
        f"--files={os.path.join(cwd, 'test_for_range.cpp')}",
        "--compilers=gcc",
        f"--output-dir={cwd}",
    ]
)

# Shadow the compiler driver with the generated wrapper
env.add_search_path(env_var="PATH", path=cwd)

# Compile our simple program
cmdrun(["gcc", "test_for_range.cpp", "-o", "test_for_range"], for_pgm=False)

# Run the executable
cmdrun(["test_for_range"], for_pgm=True)

# Check coverage expectations
xcov(
    [
        "coverage",
        "--level=stmt",
        "--sid=test_for_range.cpp.sid",
        "-axcov",
        srctracename_for("test_for_range"),
    ]
)
check_xcov_reports(".", {"test_for_range.cpp.xcov": {"+": {6, 7, 9, 11}}})

thistest.result()
