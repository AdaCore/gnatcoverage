"""
Check that gnatcov does not crash during elaboration when it fails to find
itself in PATH.

This requires a fairly specific setup for both path lookup to work in the
terminal, but not from gnatcov. So far we have managed to do it by
- Being on a Windows system
- Having the gnatcov executable being in a case-sensitive directory
- Modifying the gnatcov.exe filename to change the case of the executable
  suffix
- Invoking gnatcov without the executable suffix.

This test aims to re-create these conditions, and thus removes some stuff from
the environment to make sure we have a the right conditions.
"""

import os

from e3.os.fs import which
from e3.fs import mkdir, cp

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import run_and_log


tmp = Wdir("tmp_")

# Create an empty directory in which we'll change the case sensitivity and
# butcher the gnatcov exec.
gnatcov_install = os.path.abspath("fake_gnatcov_bin")
mkdir(gnatcov_install)

# Enable file sensitivity for the install dir. This needs to be done while the
# directory is empty.
run_and_log(
    ["fsutil.exe", "file", "setCaseSensitiveInfo", gnatcov_install, "enable"]
)

# Copy the gnatcov.exe program and change its executable suffix casing
cp(which("gnatcov.exe"), os.path.join(gnatcov_install, "gnatcov.EXE"))

# Run gnatcov without anything on the environment to make sure we don't pick up
# the correctly installed gnatcov, not the GNATCOV_PREFIX env var which would
# bypass the path lookup check. We also go through a cmd indirection to avoid
# the path lookup that takes place in e3.os.process.Run.
#
# We also need gnatcov to be invoked without the exe prefix here as the os
# search should work, but the command name for gnatcov will still not have
# the exe suffix.
crash_log = "xcov.log"
p = run_and_log(
    ["cmd", "/c", "gnatcov"],
    env={"PATH": gnatcov_install},
    output=crash_log,
    ignore_environ=True,
)

thistest.fail_if(
    p.status == 0,
    comment="gnatcov did not exit with a failure status",
)

thistest.fail_if_not_equal(
    what="Unexpected error message from gnatcov",
    expected="Could not locate the invoked gnatcov command: <empty string>."
    " If gnatcov is installed on a case sensitive filesystem or directory,"
    " ensure the casing as the executable filename is used when invoking the"
    " program.",
    actual=contents_of(crash_log).strip(),
)

thistest.result()
