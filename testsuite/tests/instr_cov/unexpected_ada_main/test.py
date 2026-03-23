"""
Check that "gnatcov instrument" emits a warning when it fails to insert the
dump of coverage buffers in mains.
"""

from SCOV.instr import xcov_instrument
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import thistest, gprfor


Wdir("tmp_")

xcov_instrument(
    gprsw=GPRswitches(root_project=gprfor(mains=["main.ads"], srcdirs=[".."])),
    covlevel="stmt",
    out="instrument.log",
    tolerate_messages=".",
)
thistest.fail_if_not_equal(
    '"gnatcov instrument" output',
    (
        "warning: cannot dump coverage buffers in main.ads:"
        " subprogram body expected"
    ),
    contents_of("instrument.log").strip(),
)

thistest.result()
