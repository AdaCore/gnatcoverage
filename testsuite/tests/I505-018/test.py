from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir, match
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


Wdir("tmp_")

# Check that "gnatcov coverage" produces reports for units exposing more than
# one entry point when compiled with -ffunction-sections.
build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(
            mains=["test_services.adb"],
            srcdirs="../src",
            compiler_extra='for Default_Switches ("Ada")'
            ' use ("-ffunction-sections");',
        )
    ),
    covlevel="stmt",
    mains=["test_services"],
    extra_coverage_args=["--annotate=xcov"],
    scos=["obj/services", "obj/test_services"],
)

# Use match to both verify that expected reports are there and
# that coverage results are as we expect as well, while we're at it.
thistest.fail_if(
    not match("100% of [1-9] lines covered", "test_services.adb.xcov")
)
thistest.fail_if(not match("-:.*raise Program_Error;", "services.adb.xcov"))

thistest.result()
