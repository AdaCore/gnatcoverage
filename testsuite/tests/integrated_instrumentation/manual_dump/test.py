"""
Ensure the manual dump trigger flag works as expected in integrated
instrumentation mode.
"""

import os

from SCOV.minicheck import check_xcov_reports
from SUITE.control import env
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import cmdrun, srctracename_for, thistest, xcov

Wdir("tmp_")

CXX = "g++"
CXX_OUTPUT = "cc.out"
BIN = "main"

# Setup the instrumentation process
xcov(
    [
        "setup-integration",
        "--level=stmt",
        "--files=../foo.cpp",
        f"--compilers={CXX}",
        "--dump-trigger=manual,../foo.cpp",
    ]
)

# Shadow the compiler driver with the generated wrapper
env.add_search_path(env_var="PATH", path=os.getcwd())

cmdrun(
    [
        CXX,
        "-o",
        BIN,
        os.path.join("..", "main.cpp"),
        os.path.join("..", "foo.cpp"),
        "-I../",
    ],
    out=CXX_OUTPUT,
    for_pgm=False,
)


thistest.fail_if_not_equal(
    "compiler output",
    "",
    contents_of(CXX_OUTPUT),
)

# Run the executable
cmdrun([BIN], for_pgm=False)

# Check coverage expectations
xcov(
    [
        "coverage",
        "--level=stmt",
        "--sid=foo.cpp.sid",
        "-axcov",
        srctracename_for(BIN),
    ]
)
check_xcov_reports(".", {"foo.cpp.xcov": {"-": {21}}})

thistest.result()
