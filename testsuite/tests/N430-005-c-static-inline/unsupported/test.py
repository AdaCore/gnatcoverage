"""
Check that gnatcov reject SCO overlapping cases with a warning.
"""

import os

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


wd = Wdir(subdir="tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(
            mains=["test_foo.c"],
            srcdirs=os.path.join("..", "src"),
            langs=["C"],
        ),
        units=["foo.c"],
    ),
    covlevel="stmt",
    mains=["test_foo"],
    extra_coverage_args=["-axcov"],
    tolerate_instrument_messages="unexpected SCO overlapping",
)

# With source traces, the warning is emitted at instrumentation time, whereas
# it is emitted at coverage time with binary traces.
#
# As SCOs source locations differ for binary and source traces, we get
# different warnings. Note that the instrumentation warning is the most
# precise.

if thistest.options.trace_mode == "src":
    output_string = "gnatcov instrument output"
    output_file = "instrument.log"
    expected_output = (
        "!!! foo.h:1:1: unexpected SCO overlapping with SCO #1: "
        "STATEMENT at foo.h:1:1-13, discarding overlapping SCO\n"
        "!!! foo.h:2:1: unexpected SCO overlapping with SCO #2: "
        "STATEMENT at foo.h:2:1-21, discarding overlapping SCO"
    )
else:
    output_string = "gnatcov coverage output"
    output_file = "coverage.log"
    expected_output = (
        "!!! foo.h:2:8: unexpected SCO overlapping with SCO #2: "
        "STATEMENT at foo.h:2:8-20, discarding overlapping SCO"
    )

thistest.fail_if_no_match(
    output_string, expected_output, contents_of(output_file)
)

thistest.result()
