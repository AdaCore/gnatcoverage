"""
Test that the output of the -M family of flags of the GCC preprocessor is not
clobbered with references to instrumentation artifacts.
"""

import os
import os.path

from e3.fs import cp

from SUITE.control import env
from SUITE.cutils import contents_of, Wdir
from SUITE.tutils import cmdrun, thistest, xcov

Wdir("tmp_")

cwd = os.getcwd()

# Copy the sources in the temporary directory
cp(os.path.join("..", "test.c"), ".")
cp(os.path.join("..", "special_0.h"), ".")

# Then, setup the instrumentation process
xcov(
    [
        "setup-integration",
        "--level=stmt",
        f"--files={os.path.join(cwd, 'test.c')}",
        "--compilers=gcc",
        f"--output-dir={cwd}",
    ]
)
baseline_dep_file = "test.dep.expected"
compile_cmd = [
    "gcc",
    "test.c",
    "-MM",
    "-MF",
    baseline_dep_file,
]

# Generate the baseline dep file with the regular compiler
cmdrun(compile_cmd, for_pgm=False)

# Shadow the compiler driver with the generated wrapper
env.add_search_path(env_var="PATH", path=cwd)

# Ask gcc to produce a dep file, using the gnatcov wrapper this time
dep_file = "test.dep.actual"
compile_cmd.pop()
compile_cmd.append(dep_file)
cmdrun(compile_cmd, for_pgm=False)

thistest.fail_if_not_equal(
    what="Difference in generated dep file",
    expected=contents_of(baseline_dep_file),
    actual=contents_of(dep_file),
)

thistest.result()
