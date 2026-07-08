"""
Check that any deviation from the expected annotation pragmas is reported to
the user as a warning. Also check that a warning is emitted if an unrecognized
Xcov annotation was found, or if an argument of the wrong type is given.
"""

import re

from e3.testsuite.driver.diff import OutputRefiner

from SCOV.minicheck import build_run_and_coverage, xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir
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


class SortOutput(OutputRefiner):

    warning_re = re.compile(r"\*\*\* ([^:]+):(\d+):.*")

    @classmethod
    def line_key(cls, line: str) -> tuple[str, int]:
        m = cls.warning_re.match(line)
        return (line, 0) if m is None else (m.group(1), int(m.group(2)))

    def refine(self, output: str) -> str:
        return "\n".join(sorted(output.splitlines(), key=self.line_key)) + "\n"


thistest.fail_if_diff(
    baseline_file="../instrument-baseline.txt",
    actual_file="instrument.log",
    failure_message="gnatcov instrument output",
    output_refiners=[SortOutput()],
)

thistest.result()
