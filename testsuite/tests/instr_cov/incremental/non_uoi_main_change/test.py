"""
Check that gnatcov reinstruments a file modified when it is not a unit of
interest, but contains the manual dump indications.
"""

from e3.fs import cp

from SCOV.minicheck import xcov_instrument
from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

# Copy the sources as we will modify main.adb
cp("../pkg.adb", ".")
cp("../pkg.ads", ".")
cp("../main.adb", ".")

root_prj = gprfor(srcdirs=["."], mains=["main.adb"])

# First instrumentation: only Pkg is a unit of interest, but main gets
# processed for manual annotations replacement.
xcov_instrument(
    gprsw=GPRswitches(root_project=root_prj),
    covlevel="stmt",
    dump_trigger="manual",
    extra_args=["--units=pkg"],
)

# Modify main.adb (which is not a unit of interest).
with open("main.adb", "a") as f:
    f.write("\n")

# Second instrumentation: main should be re-processed even though it is not a
# unit of interest, because it holds the dump code.
env.add_search_path("ADA_DEBUG_FILE", "../../.gnatdebug")
xcov_instrument(
    gprsw=GPRswitches(root_project=root_prj),
    covlevel="stmt",
    dump_trigger="manual",
    extra_args=["--units=pkg"],
    out="instrument.out",
)

thistest.fail_if_diff(
    baseline_file="../instrument.expected",
    actual_file="instrument.out",
)

thistest.result()
