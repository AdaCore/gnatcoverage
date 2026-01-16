"""
Check that any deviation from the expected annotation pragmas is reported to
the user as a warning. Also check that a warning is emitted if an unrecognized
Xcov annotation was found, or if an argument of the wrong type is given.
"""

from SCOV.minicheck import build_run_and_coverage, xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

Wdir("tmp_")

# Check gnatcov's behaviour upon encountering annotation pragmas with correctly
# typed arguments.

build_run_and_coverage(
    gprsw=GPRswitches(
        gprfor(
            prjid="test",
            srcdirs=[".."],
            mains=["main_1.adb"],
            langs=["Ada"],
            extra='for Source_Files use ("main_1.adb");',
        )
    ),
    covlevel="stmt",
    trace_mode="src",
    mains=["main_1"],
    extra_coverage_args=["--annotate=report"],
    tolerate_instrument_messages=".",
)

# Check that gnatcov does not raise an exception when finding incorrect
# annotation pragmas. In these cases, it is the compiler that is expected to
# raise an exception.

xcov_instrument(
    gprsw=GPRswitches(
        gprfor(
            prjid="test",
            srcdirs=[".."],
            mains=["main_1.adb", "main_2.adb"],
            langs=["Ada"],
        )
    ),
    covlevel="stmt",
    tolerate_messages=".",
)

thistest.fail_if_not_equal(
    "gnatcov instrument output",
    (
        "*** main_1.adb:5:5: warning: No justification given for exempted"
        " region\n"
        "*** main_1.adb:9:5: warning: At most 2 pragma arguments allowed\n"
        "*** main_1.adb:10:5: warning: Invalid Xcov annotation kind: aaa\n"
        "*** main_1.adb:11:5: warning: Xcov annotation kind missing\n"
        "*** main_2.adb:4:5: warning: Invalid Xcov annotation kind\n"
        "*** main_2.adb:6:5: warning: Invalid justification argument:"
        " static string expression expected\n"
    ),
    contents_of("instrument.log"),
)

thistest.result()
