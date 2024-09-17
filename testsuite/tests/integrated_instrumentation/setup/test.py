"""
Regression test for integrated instrumentation approach, using an
instrumentation runtime built and installed with `gnatcov setup
--restricted-to-languages=c` prior. The setup-integration command used to crash
with a `Could not load the coverage runtime project gnatcov_rts` message.
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

# Copy the sources and the Makefile in the temporary directory
cp(os.path.join("..", "Makefile"), ".")
cp(os.path.join("..", "test.c"), ".")
cp(os.path.join("..", "pkg.c"), ".")

# Install the instrumentation runtime with `--restricted-to-languages=C`
xcov(
    [
        "setup",
        "--prefix=install",
        "--restricted-to-languages=c",
        "--install-name=gnatcov_rts_c",
    ]
)
env.add_search_path("GPR_PROJECT_PATH", "install/share/gpr")

# Then, setup the instrumentation process. Do not pass --config, as that this
# was necessary to reproduce the original bug. Do not pass --target neither, as
# "gnatcov setup-integration" does not support it: let GPR code pick up the
# right native compiler from the environment and thus correctly guess the
# target.
xcov(
    [
        "setup-integration",
        "--level=stmt",
        f"--files={os.path.join(cwd, 'pkg.c')}",
        "--compilers=gcc",
        f"--output-dir={cwd}",
        "--runtime-project=gnatcov_rts_c",
    ],
    auto_config_args=False,
    auto_target_args=False,
)

# Shadow the compiler driver with the generated wrapper
env.add_search_path(env_var="PATH", path=cwd)

# Then, run the build process unchanged
cmdrun(["make", "test"], for_pgm=False)

# Run the executable
cmdrun(["test"], for_pgm=False)

# Check coverage expectations
xcov(
    [
        "coverage",
        "--level=stmt",
        "--sid=pkg.c.sid",
        "-axcov",
        srctracename_for("test"),
    ]
)
check_xcov_reports(".", {"pkg.c.xcov": {"+": {4}}})

thistest.result()
