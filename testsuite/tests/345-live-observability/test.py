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

OUTPUT = contents_of("main_output.txt")

thistest.fail_if_no_match(
    "Wrong first buffer sum", re.compile(r".*First: *2.*", re.S), OUTPUT
)

thistest.fail_if_no_match(
    "Wrong second buffer sum", re.compile(r".*Second: *4.*", re.S), OUTPUT
)

thistest.fail_if_no_match(
    "Wrong third buffer sum", re.compile(r".*Third: *8.*", re.S), OUTPUT
)

thistest.result()
