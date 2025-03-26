"""
This test ensures that the live observability of coverage data provides
sound data.
"""

import re

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

wd = Wdir("tmp_")


build_and_run(
    gprsw=GPRswitches(
        root_project=gprfor(mains=["main.adb"], srcdirs=["../src"]),
    ),
    mains=["main"],
    covlevel="stmt",
    extra_gprbuild_args=[],
    extra_coverage_args=[],
)

# In case block instrumentation is enabled, the number of bits set to 1 in the
# buffers is not equivalent to the number of statements executed.
if thistest.options.block:
    counts = [0, 0, 3]
else:
    counts = [2, 4, 8]

OUTPUT = contents_of("main_output.txt")

thistest.fail_if_no_match(
    "Wrong first buffer sum",
    re.compile(f".*First: *{counts[0]}.*", re.S),
    OUTPUT,
)

thistest.fail_if_no_match(
    "Wrong second buffer sum",
    re.compile(f".*Second: *{counts[1]}.*", re.S),
    OUTPUT,
)

thistest.fail_if_no_match(
    "Wrong third buffer sum",
    re.compile(f".*Third: *{counts[2]}.*", re.S),
    OUTPUT,
)

thistest.result()
