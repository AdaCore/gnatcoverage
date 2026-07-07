"""
Check that gnatcov only reinstruments the modified source.
"""

from e3.fs import cp

from SCOV.minicheck import xcov_instrument
from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

# Copy the sources as we will modify them
cp("../pkg.adb", ".")
cp("../pkg.ads", ".")
cp("../main.adb", ".")

# Create a projec
root_prj = gprfor(srcdirs=["."], mains=["main.adb"])
# The first instrumentation command should instrument all of the files
xcov_instrument(
    gprsw=GPRswitches(root_project=root_prj),
    covlevel="stmt",
)

# Make a file modification
with open("pkg.adb", "a") as f:
    f.write("\n")

# Check that gnatcov only instruments the Pkg unit
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
