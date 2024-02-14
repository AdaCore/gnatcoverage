"""
Regression test: checks that gnatcov accounts for the isystem switch. It used
to plainly ignore it, which resulted in preprocessing failures.
"""

import os
import os.path

from SUITE.control import env
from SUITE.cutils import contents_of, Wdir
from SCOV.minicheck import check_xcov_reports
from SUITE.tutils import cmdrun, srctracename_for, thistest, xcov

Wdir("tmp_")

# Setup the instrumentation process
xcov(
    [
        "setup-integration",
        "--level=stmt",
        "--files=../test.c",
        "--compilers=gcc",
    ]
)

# Shadow the compiler driver with the generated wrapper
env.add_search_path(env_var="PATH", path=os.getcwd())

# Then, run the compile + link command
gcc_output = "gcc.out"
cmdrun(
    [
        "gcc",
        "-o",
        "test",
        os.path.join("..", "test.c"),
        "-isystem",
        os.path.join("..", "include"),
    ],
    out=gcc_output,
    for_pgm=False,
)

thistest.fail_if_not_equal(
    "compiler output",
    "",
    contents_of(gcc_output),
)

# Run the executable
cmdrun(["test"], for_pgm=False)

# Check coverage expectations
xcov(
    [
        "coverage",
        "--level=stmt",
        "--sid=test.c.sid",
        "-axcov",
        srctracename_for("test"),
    ]
)
check_xcov_reports(".", {"test.c.xcov": {"+": {6, 7}}})

thistest.result()
