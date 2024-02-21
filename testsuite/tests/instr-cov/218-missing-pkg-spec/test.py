"""
Check that "gnatcov instrument" does not crash when instrumenting an invalid
project setup that lacks the package spec corresponding to a package body.
"""

import os.path

from e3.fs import mkdir

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir, lines_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

# Avoid "creating output path" messages from gnatcov
mkdir("obj")

filename = "instrument.txt"
p = xcov_instrument(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.adb"])),
    covlevel="stmt",
    out=filename,
    register_failure=False,
)

thistest.fail_if(p.status != 0, "'gnatcov instrument' unexpectedly succeeded")

# There are two warnings that we consider mandatory. Beyound that, we tolerate
# other warnings only (to keep this test robust, we do not want to track the
# exact list of warnings, that may vary as gnatcov evolves).
expected_warnings = {
    "warning: While instrumenting main.adb...",
    "warning: Cannot find required source file: pkg.ads",
}
actual_warnings = set()
others = []
for line in lines_of(filename):
    if line.startswith("warning: ") or line.startswith("*** warning:"):
        actual_warnings.add(line)
    else:
        others.append(line)

missing_warnings = expected_warnings - actual_warnings
thistest.fail_if(
    missing_warnings,
    'expected "gnatcov instrument" warnings are missing:\n'
    + "\n".join(sorted(missing_warnings)),
)

thistest.fail_if_not_equal(
    '"gnatcov instrument" has non-warnings in its output',
    "",
    "\n".join(others),
)

thistest.result()
