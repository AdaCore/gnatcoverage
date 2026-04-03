"""
Check that "gnatcov instrument" warns about missing files.
"""

import re

from e3.fs import mkdir

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir, lines_of
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
        "warning: Cannot find required source file: bar.ads"
    ),
    # This test intentionally misses source files, so the instrumenter gets
    # errors from Libadalang: ignore them for the purpose of this test.
    actual="\n".join(
        line
        for line in lines_of("instrument.log")
        if not re.match(r"\*\*\* (baz|foo).adb:\d+:\d+: warning: ", line)
    ),
)

thistest.result()
