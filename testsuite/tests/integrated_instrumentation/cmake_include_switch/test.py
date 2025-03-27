"""
Regression test: check that the gcc compiler wrapper correctly discards the
different variations of the include switch when invoking the original compiler
driver command.
"""

import os
import os.path

from SUITE.cutils import Wdir
from SCOV.minicheck import check_xcov_reports
from SUITE.tutils import cmdrun, srctracename_for, thistest, xcov


def process(c_flags, subdir):
    # Run the integrated instrumentation process with the given c_flags, and in
    # the given subdir
    tmp = Wdir(subdir)
    cwd = os.getcwd()
    # Setup the instrumentation process
    xcov(
        [
            "setup-integration",
            "--level=stmt",
            f"--files={os.path.join(cwd, '..', 'main.c')}",
            f"--files={os.path.join(cwd, '..','pkg.h')}",
            "--compilers=gcc",
        ]
    )
    # Run the build + coverage process with the given c_flags
    compiler_wrapper = os.path.join(cwd, "gcc")
    cmdrun(
        [
            "cmake",
            "..",
            f"-DCMAKE_C_COMPILER={compiler_wrapper}",
            f'-DCMAKE_C_FLAGS={" ".join(c_flags)}',
        ],
        for_pgm=False,
    )
    cmdrun(["make"], for_pgm=False)

    # Run the executable
    cmdrun(["./hello_world"], for_pgm=False)

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
    check_xcov_reports(
        ".", {"main.c.xcov": {"+": {5, 6}}, "pkg.h.xcov": {"+": {4}}}
    )
    tmp.to_homedir()


header = os.path.join(os.getcwd(), "pkg.h")
process(["-include", header], "tmp_short_space_arg")
process([f"-include{header}"], "tmp_short_no_space_arg")
process(["--include", header], "tmp_long_space_arg")
process([f"--include={header}"], "tmp_long_eq_arg")

thistest.result()
