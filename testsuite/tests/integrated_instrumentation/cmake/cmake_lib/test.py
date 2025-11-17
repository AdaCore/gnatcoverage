"""
Test that gnatcov correctly takes into account instrumented units present
in a static library passed as a positional argument on the link command line.
This is what is done by CMake when defining a library, then using it in an
executable in the same CMakeLists.txt.
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
        f"--files={os.path.join(cwd, '..', 'src', 'main.c')}",
        f"--files={os.path.join(cwd, '..', 'src', 'lib.c')}",
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
cmdrun(["hello_world"], for_pgm=True)

# Check coverage expectations
xcov(
    [
        "coverage",
        "--level=stmt",
        "--sid=main.c.sid",
        "--sid=lib.c.sid",
        "-axcov",
        srctracename_for("main"),
    ]
)
check_xcov_reports(
    ".",
    {
        "main.c.xcov": {"+": {8, 9}},
        "lib.c.xcov": {"+": {6}},
    },
)

thistest.result()
