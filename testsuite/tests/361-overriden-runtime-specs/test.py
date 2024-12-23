"""
This test checks that we have no warning when instrumenting a project
that redefines some runtime files, here a-convec.ad[sb]
"""

import re

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor

wd = Wdir("tmp_")

gpr = gprfor(mains=["main.adb"], srcdirs=["../src"])

build_and_run(
    gprsw=GPRswitches(
        root_project=gpr,
    ),
    mains=["main"],
    covlevel="stmt",
    extra_coverage_args=["-v"],
    quiet=False,
)

# Ensure `gnatcov instrument` did not confuse runtime files
warn_regexp = re.compile(
    r".*Warning: same base name for files:\n.*a-convec\.ads\n.*a-convec\.ads.*",  # noqa: E501
    flags=re.S,
)
thistest.fail_if_match(
    "Warning found in instrumentation output, gnatcov used both overriden "
    "and original runtime files",
    warn_regexp,
    contents_of("instrument.log"),
)

# Ensure the overridden runtime file was used
# (i.e. calling Vector.Append prints Toto)
thistest.fail_if_no_match(
    "call to Vector.Append shall print 'Toto'",
    "Toto",
    contents_of("main_output.txt"),
)

thistest.result()
