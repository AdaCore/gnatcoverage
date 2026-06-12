"""
Regression testcase: check that gnatcov passes -std=gnu<std_version> to the
clang parsing invocation when using a GNU compiler rather than
-std=c<std_version>. The latter results in clang falling back to the default
implementation for the assert macro, ultimately resulting in different SCOs
between the recording preprocessing information step (where the original code
is parsed by clang) and the instrumentation step (where clang parses the
preprocessed code).
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

Wdir("tmp_")
build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(
            srcdirs=[".."],
            mains=["main.c"],
        )
    ),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)

check_xcov_reports("xcov", {"main.c.xcov": {"+": {6, 7}}})

thistest.result()
