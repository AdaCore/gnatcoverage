"""
Regression test: test that gnatcov correctly compile instrumentation artifacts
with the architecture-specific switches (such as -m32) passed to the original
compiler driver invocation. It used not to, which resulted in an error when
linking the instrumentation artifacts with the instrumented source, which was
compiled with them.
"""

import os
import os.path

from SUITE.control import env
from SUITE.cutils import Wdir
from SUITE.tutils import cmdrun, thistest, xcov

Wdir("tmp_")

# Setup the instrumentation process
xcov(
    [
        "setup-integration",
        "--level=stmt",
        "--files=../pkg.c",
        "--compilers=gcc",
    ]
)

# Shadow the compiler driver with the generated wrapper
env.add_search_path(env_var="PATH", path=os.getcwd())

# Try to compile the source. The test used to fail there.
cmdrun(["gcc", "-m32", "../pkg.c", "-c"], for_pgm=False)

thistest.result()
