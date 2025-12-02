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

import os.path

from e3.fs import cp

from SUITE.cutils import Wdir
from SCOV.minicheck import check_xcov_reports
from SUITE.integrated_instr_utils import MakefileMain, setup_integration
from SUITE.tutils import (
    cmdrun,
    run_cov_program,
    srctracename_for,
    thistest,
    xcov,
)

Wdir("tmp_")

# Copy the sources in the temporary directory
cp(os.path.join("..", "main.c"), ".")
cp(os.path.join("..", "pkg.c"), ".")

# Copy the fake ldd in the temporary directory
cp(os.path.join("..", "ldd"), ".")

# Then, setup the instrumentation process
env = setup_integration(files_of_interest=["pkg.c"])
make_wf = MakefileMain(build_target_deps=["main.o", "pkg.o"])
make_wf.build(env=env)

# Remove our fake ldd to avoid shadowing the system one at execution time
cmdrun(["rm", "ldd"], for_pgm=False)

# Run the executable
run_cov_program("./main")

# Check coverage expectations
xcov(
    [
        "coverage",
        "--level=stmt",
        "--sid=pkg.c.sid",
        "-axcov",
        srctracename_for("main"),
    ]
)
check_xcov_reports(".", {"pkg.c.xcov": {"+": {4}}})

thistest.result()
