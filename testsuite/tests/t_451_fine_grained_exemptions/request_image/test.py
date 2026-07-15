"""
Check that gnatcov formats human readable descriptions of exemption requests
correctly.
"""

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

gpr = gprfor(srcdirs=[".."], mains=["main.adb"])

warnings = []
for msg in [
    "Duplicate exemption at main.adb:6:7 for outcome FALSE of decision #1",
    "Duplicate exemption at main.adb:6:7 for condition 0 of decision #1",
    "Duplicate exemption at main.adb:6:7 for all outcomes and decisions of"
    " decision #1",
    "Duplicate manual evaluation at main.adb:6:7 of decision #1: T F",
]:
    warnings += [
        f"*** main.adb:6:7: warning: {msg}",
        "*** main.adb:6:7: warning: Discarding justification: M2",
        "*** main.adb:6:7: warning: In favor of: M1",
    ]

log_filename = "instrument.log"
xcov_instrument(
    gprsw=GPRswitches(root_project=gpr),
    covlevel="stmt+decision",
    extra_args=["--external-annotations", "../annotations.toml"],
    out=log_filename,
    tolerate_messages=".*",
)

baseline_filename = "expected.txt"
with open(baseline_filename, "w") as f:
    for line in warnings:
        print(line, file=f)
thistest.fail_if_diff(
    baseline_filename,
    log_filename,
    '"gnatcov coverage" output',
)

thistest.result()
