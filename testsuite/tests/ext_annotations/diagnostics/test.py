"""
Test that gnatcov emits the expected diagnostics when loading external
annotations, for unknown annotations kinds or ill-formed annotation
payloads.
"""

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

tmp = Wdir("tmp_")

prj = gprfor(mains=["main.adb"], srcdirs=[".."])

log = "instr.log"

# Instrument the project with all the external exemptions
xcov_instrument(
    gprsw=GPRswitches(root_project=prj),
    covlevel="stmt",
    dump_trigger="manual",
    extra_args=[
        "--external-annotations=../buffers.toml",
        "--external-annotations=../exemptions.toml",
        "--external-annotations=../load_fail.toml",
        "--external-annotations=../cov_off.toml",
    ],
    tolerate_messages=".*",
    out=log,
)


thistest.fail_if_diff(
    "../expected.txt",
    log,
    "Unexpected messages for 'gnatcov instrument' output",
)

thistest.result()
