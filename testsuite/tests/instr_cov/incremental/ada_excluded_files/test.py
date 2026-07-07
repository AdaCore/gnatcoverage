"""
Check that gnatcov reinstruments a unit when instrumenting every part of the
unit after it has been partially instrumented (by using
--excluded-source-files).
"""

from e3.fs import cp

from SCOV.minicheck import xcov_instrument
from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

# Copy the sources in the current directory
cp("../pkg.adb", ".")
cp("../pkg.ads", ".")
cp("../main.adb", ".")

root_prj = gprfor(srcdirs=["."], mains=["main.adb"])

# The first instrumentation command instruments the Pkg unit partially: the
# spec is not instrumented.
xcov_instrument(
    gprsw=GPRswitches(root_project=root_prj),
    covlevel="stmt",
    extra_args=["--excluded-source-files=pkg.ads"],
)

# Now, do not pass --excluded-source-files: gnatcov should reinstrument the
# Pkg unit.
env.add_search_path("ADA_DEBUG_FILE", "../../.gnatdebug")
xcov_instrument(
    gprsw=GPRswitches(root_project=root_prj),
    covlevel="stmt",
    out="instrument.out",
)

thistest.fail_if_diff(
    baseline_file="../instrument.expected",
    actual_file="instrument.out",
)

thistest.result()
