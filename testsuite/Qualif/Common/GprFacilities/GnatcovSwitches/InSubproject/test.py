import os.path
import re

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import Csw, GPRswitches, gprcov_for
from SUITE.tutils import gprfor


pgm = "test_lt0"
wd = Wdir("tmp_")

# Build and run the single test program, which volontarily performs stmt and
# decision coverage violations.
#
# Enforce --level=stmt+decision here. Check that Default_Switches in
# subprojects are ignored
gpr = gprfor(
    mains=[pgm + ".adb"],
    srcdirs=["../src"],
    deps=["../App/app"],
    extra=gprcov_for(
        switches=[
            Csw("*", ["--level=stmt+decision"]),
            Csw("coverage", ["--annotate=report"]),
        ]
    ),
)
build_run_and_coverage(
    gprsw=GPRswitches(root_project=gpr, no_subprojects=False),
    covlevel=None,
    mains=[pgm],
    extra_coverage_args=["-o", "def.rep"],
)

# Check that we get results corresponding to the root project file despite
# "overrides" (other Switches) in subprojects.

rep = contents_of("def.rep")

thistest.fail_if(not os.path.exists("def.rep"), "couldn't find default report")

thistest.fail_if(
    not re.search("statement not executed", rep),
    "missing expected stmt coverage failure indication",
)

thistest.fail_if(
    not re.search(r"test.*\.adb:.*: decision outcome .* never exercised", rep),
    "missing expected decision coverage failure indication",
)

thistest.fail_if(
    not re.search("values.adb:.*: decision outcome .* never exercised", rep),
    "missing expected decision coverage failure indication",
)

thistest.result()
