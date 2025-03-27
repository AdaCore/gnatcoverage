"""
Regression test: gnatcov used to leave paths passed to the --files switch
un-normalized, which resulted in some units of interest being ignored at
instrumentation time.
"""

import os
import os.path

from SUITE.control import env
from SUITE.cutils import Wdir
from SCOV.minicheck import check_xcov_reports
from SUITE.tutils import cmdrun, srctracename_for, thistest, xcov

Wdir("tmp_")

cwd = os.getcwd()

# Setup the instrumentation process
xcov(
    [
        "setup-integration",
        f"--files={os.path.join(cwd, '..', 'main.c')}",
        "--compilers=gcc",
        "--level=stmt",
    ]
)

# Shadow the compiler driver with the generated wrapper
env.add_search_path(env_var="PATH", path=cwd)

# Then, run the build process unchanged
compiler_wrapper = os.path.join(cwd, "gcc")
cmdrun(
    ["cmake", "..", f"-DCMAKE_C_COMPILER={compiler_wrapper}"], for_pgm=False
)
cmdrun(["make"], for_pgm=False)

# Run the executable
cmdrun(["hello_world"], for_pgm=False)

# Check coverage expectations
xcov(
    [
        "coverage",
        "--level=stmt",
        "--sid=main.c.sid",
        "-axcov",
        srctracename_for("main"),
    ]
)
check_xcov_reports(".", {"main.c.xcov": {"+": {6, 7}}})

thistest.result()
