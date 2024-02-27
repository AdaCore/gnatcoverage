"""
Check that any deviation from the expected annotation pragmas is reported to
the user as a warning. Also check that a warning is emitted if an unrecognized
Xcov annotation was found, and that when an argument of the wrong type is
given it is gprbuild and gnatcov that raises an exception.
"""

from SCOV.minicheck import build_run_and_coverage, xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

Wdir('tmp_')

# Check gnatcov's behaviour upon encountering annotaton pragmas with correctly
# types arguments.

build_run_and_coverage(
    gprsw=GPRswitches(
        gprfor(prjid="test",
               srcdirs=[".."],
               mains=["main_1.adb"],
               langs=["Ada"])),
    covlevel="stmt",
    trace_mode="src",
    mains=["main_1"],
    extra_coverage_args=["--annotate=report"],
    tolerate_instrument_messages=".")

# Check that gnatcov does not raise an exception when finding incorrect
# annotation pragmas. In these cases, it is the compiler that is expected to
# raise an exception.

xcov_instrument(
    gprsw=GPRswitches(
        gprfor(prjid="test",
               srcdirs=[".."],
               mains=["main_2.adb"],
               langs=["Ada"])),
    covlevel="stmt",
    tolerate_messages='.')

thistest.fail_if_not_equal(
    "gnatcov instrument output",
    (
        "*** main_1.adb:5:5: warning: No justification given for exempted"
        " region\n"
        "*** main_1.adb:9:5: warning: At most 2 pragma arguments allowed\n"
        "*** main_1.adb:10:5: warning: Invalid justification argument: string"
        " literal expected\n"
        "*** main_1.adb:11:5: warning: Invalid Xcov annotation kind: aaa\n"
        "*** main_1.adb:12:5: warning: Xcov annotation kind missing\n"
        "*** main_2.adb:4:5: warning: Invalid Xcov annotation kind\n"
    ),
    contents_of("instrument.log"),
)

thistest.result()
