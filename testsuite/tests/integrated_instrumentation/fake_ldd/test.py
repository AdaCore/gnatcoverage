"""
Regression test for integrated instrumentation, with a fake ldd that yields the
following output:
    linux-vdso.so.1 => (0x00007ffda3d11000)
    libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007d8894a00000)
    /lib64/ld-linux-x86-64.so.2 (0x00007d8894dcc000)

The difference with ldd is the first line, which is normally displayed as:
    linux-vdso.so.1 (0x00007ffda3d11000)

As gnatcov relies on the contents after the arrow, and before the loading
address to get the library path, it grabbed an invalid name in the former case.
There was no consistency check, which resulted in the compiler wrapper
crashing.
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

# Copy the fake ldd in the temporary directory
cp(os.path.join("..", "ldd"), ".")

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

# Put the compiler wrapper and the fake ldd on the PATH
env.add_search_path(env_var="PATH", path=cwd)

# Then, run the build process unchanged
cmdrun(["make", "test"], for_pgm=False)

# Remove our fake ldd to avoid shadowing the system one at execution time
cmdrun(["rm", "ldd"], for_pgm=False)

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
