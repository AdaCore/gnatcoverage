"""
Test that compiler output is correctly forwarded to the user on error.
"""

import os
import re

from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import cmdrun, xcov


Wdir("tmp_")

# Setup the instrumentation process
with open("main.c", "w"):
    pass
xcov(
    [
        "setup-integration",
        "--level=stmt",
        "--files=main.c",
        "--compilers=gcc",
        "--output-dir=.",
    ]
)

# Shadow the compiler driver with the generated wrapper
env.add_search_path(env_var="PATH", path=os.getcwd())

# Run the GCC wrapper with an invalid flag and check its output
output_file = "gcc-output.txt"
cmdrun(
    ["gcc", "-c", "main.c", "-invalid-switch"],
    out=output_file,
    for_pgm=False,
    expect_non_zero_code=True,
)

output = contents_of(output_file)
error_msg = "gcc: error: unrecognized command-line option '-invalid-switch'"
thistest.fail_if_no_match(
    "gcc wrapper output", f"(.|\n)*{re.escape(error_msg)}(.|\n)*", output
)

thistest.result()
