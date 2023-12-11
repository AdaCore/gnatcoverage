"""
Regression test-case: checks that the use of the compiler wrapper is
transparent to the user, meaning that all adequate use of the compiler are
supported by the compiler wrapper, and not only compiling / linking commands.
gnatcov used to crash on e.g. `gcc --version`. Also check other special cases
for robustness.
"""

import os
import os.path

from SUITE.control import env
from SUITE.cutils import contents_of, Wdir
from SUITE.tutils import cmdrun, thistest, xcov

Wdir("tmp_")

# Setup the instrumentation process
xcov(
    [
        "setup-integration",
        "--level=stmt",
        f"--files={os.path.join('..', 'pkg.c')}",
        "--compilers=gcc",
    ]
)

# Shadow the compiler driver with the generated wrapper
env.add_search_path(env_var="PATH", path=os.getcwd())

# Check that gcc --version do not crash
cmdrun(["gcc", "--version"], for_pgm=False)

# Check that gcc -### -c pkg.c -o pkg.o does not crash and does not produce an
# object file.
cmdrun(["gcc", "-###", "-c", "../pkg.c", "-o", "pkg.o"], for_pgm=False)
thistest.fail_if(
    os.path.exists("pkg.o"),
    "unexpected pkg.o file",
)

# Check that gcc -E pkg.c produces uninstrumented preprocessed code
cmdrun(["gcc", "-E", "../pkg.c", "-o", "pkg.pp"], for_pgm=False)
thistest.fail_if(
    "witness" in contents_of("pkg.pp"),
    "unexpected instrumented preprocessed file",
)

thistest.result()
