"""
Check that gnatcov does not break compilation with the -pedantic switch. The
code generated for an empty buffers list used to be uncompilable with
-pedantic. Note that -pedantic should only diagnose warnings and not errors,
so this is not expected, but we can work around the issue easily so fix the
wrong code.
"""

import os
import os.path

from e3.fs import cp

from SUITE.control import env
from SUITE.cutils import Wdir
from SCOV.minicheck import check_xcov_reports
from SUITE.tutils import cmdrun, srctracename_for, thistest, xcov


# To avoid source repository pollution and interference between successive test
# runs, copy source material to a temporary directory and run the test there,
# which is common practice in the gnatcov testsuite. Doing so is however
# absolutely not a requirement to use the integrated instrumentation scheme.
Wdir("tmp_")

src_dir = os.path.abspath("src")
run_dir = os.path.abspath("run")
os.mkdir(src_dir)
os.mkdir(run_dir)

for filename in ["Makefile", "pkg.c", "test.c"]:
    cp(os.path.join("..", filename), os.path.join(src_dir, filename))

# Then, setup the instrumentation process
xcov(
    [
        "setup-integration",
        "--level=stmt",
        "--compilers=g++",
        f"--output-dir={run_dir}",
    ]
)

# Shadow the compiler driver with the generated wrapper
env.add_search_path(env_var="PATH", path=run_dir)

# Then, run the build process unchanged
cmdrun(["make", "-C", src_dir, "test"], for_pgm=False)

# Run the executable
cmdrun([os.path.join(src_dir, "test")], for_pgm=False)

thistest.result()
