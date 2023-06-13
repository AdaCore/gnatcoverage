"""
Check that "gnatcov instrument" warns about missing files.
"""

from e3.fs import mkdir

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")
gpr = gprfor(mains=["foo.adb"], srcdirs=[".."])
mkdir("obj")

xcov_instrument(
    gprsw=GPRswitches(root_project=gpr),
    covlevel="stmt",
    out="instrument.log",
    tolerate_messages=".",
)

thistest.fail_if_not_equal(
    what="'gnatcov instrument' output",
    expected=(
        "warning: While instrumenting baz.adb...\n"
        "warning: Cannot find required source file: bar.ads\n"
    ),
    actual=contents_of("instrument.log"),
)

thistest.result()
