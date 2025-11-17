"""
Regression test-case: checks that the use of the compiler wrapper is
transparent to the user, meaning that all adequate use of the compiler are
supported by the compiler wrapper, and not only compiling / linking commands.
gnatcov used to crash on e.g. `gcc --version`. Also check other special cases
for robustness.
"""

import os
import os.path

from SUITE.cutils import contents_of, Wdir
from SUITE.integrated_instr_utils import (
    CompileSource,
    RunCompiler,
    setup_integration,
)
from SUITE.tutils import thistest

Wdir("tmp_")

# Setup the instrumentation process
env = setup_integration(files_of_interest=[("../pkg.c")])

# Check that gcc --version do not crash
RunCompiler(switches=["--version"]).build(env=env)

# Check that gcc -### -c pkg.c -o pkg.o does not crash and does not produce an
# object file.
CompileSource(compiler_switches=["-###"], source="../pkg.c").build(env=env)
thistest.fail_if(
    os.path.exists("pkg.o"),
    "unexpected pkg.o file",
)

# Check that gcc -E pkg.c produces uninstrumented preprocessed code
RunCompiler(switches=["-E", "../pkg.c", "-o", "pkg.pp"]).build(env=env)
thistest.fail_if(
    "witness" in contents_of("pkg.pp"),
    "unexpected instrumented preprocessed file",
)

thistest.result()
