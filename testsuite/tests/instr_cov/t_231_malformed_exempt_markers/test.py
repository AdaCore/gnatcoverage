"""
Check that any deviation from the expected annotation pragmas is reported to
the user as a warning. Also check that a warning is emitted if an unrecognized
Xcov annotation was found, or if an argument of the wrong type is given.
"""

import re

from SCOV.minicheck import build_run_and_coverage, xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir, lines_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

Wdir("tmp_")

# Check gnatcov's behaviour upon encountering annotation pragmas with correctly
# typed arguments.

build_run_and_coverage(
    gprsw=GPRswitches(
        gprfor(
            prjid="test1",
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
            prjid="test2",
            srcdirs=[".."],
            mains=["main_1.adb", "main_2.adb", "main_3.c"],
            langs=["Ada", "C"],
        )
    ),
    covlevel="stmt",
    tolerate_messages=".",
)

warning_re = re.compile(r"\*\*\* ([^:]+):(\d+):.*")


def line_key(line: str) -> tuple[str, int]:
    m = warning_re.match(line)
    return (line, 0) if m is None else (m.group(1), int(m.group(2)))


baseline = """\
*** main_1.adb:5:5: warning: No justification given for exempted region
*** main_1.adb:9:40: warning: Too many arguments
*** main_1.adb:10:5: warning: Invalid Xcov annotation kind: aaa
*** main_1.adb:11:5: warning: Xcov annotation kind missing
*** main_2.adb:4:5: warning: Invalid Xcov annotation kind
*** main_2.adb:6:39: warning: Static string expression expected
*** main_2.adb:7:39: warning: Invalid argument name: no_such_arg
*** main_2.adb:8:46: warning: Too many arguments
*** main_3.c:4:5: warning: Invalid Xcov annotation kind: GNATCOV_INVALID_KIND
*** main_3.c:5:23: warning: Invalid syntax
*** main_3.c:6:24: warning: Invalid syntax
*** main_3.c:7:33: warning: unexpected end of annotation
*** main_3.c:8:24: warning: String expected
*** main_3.c:9:31: warning: Too many arguments
*** main_3.c:10:24: warning: Invalid argument name: no_such_arg
*** main_3.c:11:6: warning: Obsolete syntax, support will be removed in\
 release 28. Consider switching to: GNATCOV_EXEMPT_ON("legacy")
"""

thistest.fail_if_not_equal(
    "gnatcov instrument output",
    baseline,
    "\n".join(sorted(lines_of("instrument.log"), key=line_key)) + "\n",
)

thistest.result()
