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

# For windows, the OS looks for the DLL in the PATH, and for Linux, it looks
# in the LD_LIBRARY_PATH.
if thistest.env.target.os.name == "windows":
    so_path = "PATH"
    so_name = "libfoobar.dll"
else:
    so_path = "LD_LIBRARY_PATH"
    so_name = "libfoobar.so"

# Check that when running the build process without LD_LIBRARY_PATH set,
# gnatcov warns that it cannot find the shared library dependency (because it
# relies on ldd, which looks at the LD_LIBRARY_PATH to know the shared library
# location).
cmdrun(["make"], out="make.out", for_pgm=False)
thistest.fail_if(
    f"warning: Could not find library {so_name}. Add its directory to"
    f" the {so_path} if this is an instrumented library."
    not in contents_of("make.out"),
    "Missing warning in make output",
)

# Then, run the build process
env.add_search_path(
    so_path,
    os.path.join(cwd, "lib"),
)
cmdrun(["make", "clean"], for_pgm=False)
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
check_xcov_reports(".", {"bar.c.xcov": {"+": {4}}, "foo.c.xcov": {"+": {4}}})

thistest.result()
