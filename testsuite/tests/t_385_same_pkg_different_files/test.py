"""
foo.ads declares the same package as pkg.ads but with a different interface. It
is not used and probably an error in the clients' code, but it does not disturb
non-instrumented builds, so it shall not disturb instrumented builds either.
"""

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

gpr = gprfor(
    mains=["main.adb"],
    srcdirs=[".."],
)

cov_args = build_and_run(
    gprsw=GPRswitches(root_project=gpr),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=[],
    tolerate_instrument_messages=r"warning: unit name \"PKG\" does not match"
    " source name.*",
)

thistest.result()
