"""
Check that gnatcov correctly uses the Switches attributes from the
project designated by the Origin_Project attribute in the root project.
"""

import os

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

# Generate a project file for our code, containing all the relevant
# coverage switches in the Coverage package Switches attributes.
code_prj = gprfor(
    mains=[],
    prjid="code",
    srcdirs=[os.path.join("..", "src_code")],
    extra="package Coverage is"
    '\n   for Switches ("*") use ("--level=stmt+mcdc");'
    '\n   for Switches ("coverage") use ("-axcov", "--output-dir=xcov");'
    "\nend Coverage;",
)

# Generate a project for the test driver, "withing" the code project and
# referencing it through the Origin_Project attribute
test_prj = gprfor(
    mains=["test_main.adb"],
    prjid="test",
    srcdirs=[os.path.join("..", "src_test")],
    deps=[code_prj],
    extra=f'for Origin_Project use "{code_prj}";',
)

# No level or annotation format provided, everything should be loaded from
# the Origin_Project (code.gpr).
build_run_and_coverage(
    gprsw=GPRswitches(root_project=test_prj),
    covlevel=None,
    extra_coverage_args=[],
    mains=["test_main"],
)

check_xcov_reports("xcov", {"code.ads.xcov": {}, "code.adb.xcov": {"!": {4}}})

thistest.result()
