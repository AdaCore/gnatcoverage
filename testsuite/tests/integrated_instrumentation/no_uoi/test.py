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

Wdir("tmp_")

cwd = os.getcwd()

# Then, setup the instrumentation process
xcov(
    [
        "setup-integration",
        "--level=stmt",
        "--compilers=g++",
        f"--output-dir={cwd}",
    ]
)

# Shadow the compiler driver with the generated wrapper
env.add_search_path(env_var="PATH", path=cwd)

# Then, run the build process unchanged
cmdrun(["make", "-C", "..", "test"], for_pgm=False)

# Run the executable
cmdrun(["../test"], for_pgm=False)

thistest.result()
