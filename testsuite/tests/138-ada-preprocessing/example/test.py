"""
Check that the instrumentation of Ada sources with preprocessing enabled works
as expected on an example project.
"""

import os
import os.path

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")

# Avoid "creating output path" info messages
os.mkdir("obj")

# Test the working case. The "log" preprocessing symbol is set to "false" in
# "prep.txt", so all logging lines are supposed to be disabled and thus not
# create coverage obligations. Yet the line numbers for the code remaining are
# supposed to be preserved.
thistest.log("== Up to the coverage report ==")
build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(
            mains=["test_eval.adb"],
            srcdirs=[".."],
            compiler_extra=(
                'for Default_Switches ("Ada")'
                ' use ("-gnatep=" & Project\'Project_Dir & "/../prep.txt");'
            ),
        )
    ),
    covlevel="stmt+decision",
    mains=["test_eval"],
    extra_coverage_args=["-axcov", "--output-dir=xcov"],
    trace_mode="src",
)
check_xcov_reports(
    "*.xcov",
    {
        "test_eval.adb.xcov": {"+": {4}, "!": {12}, "-": {13}},
        "vm.ads.xcov": {"+": {3, 4, 6, 7, 16, 17}},
        "vm.adb.xcov": {
            "+": {
                # Eval header
                13,

                # Pop
                27, 31,

                # Push
                43, 44,

                # Eval loop
                61, 62, 70, 72, 87, 89, 90, 96,

                # Eval wrapper
                117, 118, 121, 122, 123, 125, 126, 127,
            },
            "!": {
                # Branch condition evaluation
                78
            },
            "-": {
                # Jump
                75,

                # Branch jump
                79,

                # Push_Lit, Add
                83, 94,
            },
        },
    },
    cwd="xcov",
)

thistest.result()
