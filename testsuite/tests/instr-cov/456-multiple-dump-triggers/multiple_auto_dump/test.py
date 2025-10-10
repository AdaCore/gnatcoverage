"""
Test that providing several automatic `--dump-trigger`s results in an error if
not identical
"""

from e3.os.process import PIPE

from SCOV.minicheck import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

p = gprfor(
    mains=["main.adb"],
    srcdirs=["../src"],
)

thistest.comment("main-end + manual + main-end should be fine")
process = xcov_instrument(
    gprsw=GPRswitches(root_project=p, units=["main"]),
    covlevel="stmt",
    dump_trigger=["main-end", "manual", "main-end"],
)
thistest.fail_if_not_equal(
    "Giving the same automatic dump-trigger twice should work",
    0,
    process.status,
)

thistest.comment("atexit + manual + main-end should ERROR")
process = xcov_instrument(
    gprsw=GPRswitches(root_project=p, units=["main"]),
    covlevel="stmt",
    dump_trigger=["atexit", "manual", "main-end"],
    register_failure=False,
    err=PIPE,
)

thistest.fail_if_no_match(
    "Wrong error message",
    r".*Encountered --dump-trigger=main-end after --dump-trigger=atexit."
    " Multiple auto dump triggers are not supported",
    process.err,
)
thistest.fail_if(
    0 == process.status, f"Expected non-zero exit code, got {process.status}"
)

thistest.result()
