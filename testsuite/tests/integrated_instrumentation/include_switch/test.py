"""
Regression test: when the user passed the -include switch to gcc, it was
processed both by gnatcov when instrumenting the file and by gcc when compiling
the instrumented file. This resulted in reincluding the file, which could yield
a compilation error. GNATcov now passes the -fpreprocessed switch to the gcc
invocation, which means it now longer tries to preprocess the instrumented file
(that is already preprocessed).
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

# Copy the sources in the temporary directory
cp(os.path.join("..", "test.c"), ".")
cp(os.path.join("..", "glob.h"), ".")

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

# Shadow the compiler driver with the generated wrapper
env.add_search_path(env_var="PATH", path=cwd)

# Then, compile and link. Test all the various way of passing a -include file
cmdrun(["gcc", "-include", "glob.h", "test.c", "-o", "test"], for_pgm=False)
cmdrun(["gcc", "--include", "glob.h", "test.c", "-o", "test"], for_pgm=False)
cmdrun(["gcc", "--include=glob.h", "test.c", "-o", "test"], for_pgm=False)
cmdrun(["gcc", "-include./glob.h", "test.c", "-o", "test"], for_pgm=False)

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
check_xcov_reports(".", {"test.c.xcov": {"+": {4}}})

thistest.result()
