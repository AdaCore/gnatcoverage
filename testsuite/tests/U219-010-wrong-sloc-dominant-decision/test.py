"""
Check that gnatcov does not crash on wrong source location for dominance
information.
"""

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


tmp = Wdir("tmp_")

xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."],
                                          mains=["main.adb"])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov"],
)

# Artificially create wrong source locations to check that gnatcov does
# not crash on these.

with open('obj/main.ali', 'r') as f:
    content = f.read()
    assert "CS >T5:4 6:7-6:17" in content
    content = content.replace(">T5:4", ">T5:5")

with open('obj/main.ali', 'w') as f:
    f.write(content)

xcov(xcov_args, out="coverage.log")

# Check that a warning is issued

thistest.fail_if_not_equal(
    "gnatcov coverage output",
    "!!! main.adb:6:7: dominant decision of statement SCO #4: STATEMENT "
    "at main.adb:6:7-17 has no associated SCO, discarding dominance "
    "information\n",
    contents_of("coverage.log"))

thistest.result()
