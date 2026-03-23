"""
Check that gnatcov processes the GPR_TOOL environment variable/scenario
variables as expected.
"""

import glob
import os.path

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


# Ways to convey to gnatcov a value for the GPR_TOOL variable
env_var = "env_var"  # Environment variable
x_arg = "x_arg"  # -X command-line argument

# List of scenarios to test. Each tuple contains:
#
#  1) The set of ways to convey the GPR_TOOL variable to gnatcov
#  2) The unit of interest we expect for this scenario.
#
# -X arguments take precedence over everything else. Then comes the environment
# variable, and finally, by default GPR_TOOL is supposed to be "gnatcoverage".
test_scenarios = [
    (set(), "unit_gnatcoverage"),
    ({env_var}, "unit_env_var"),
    ({x_arg}, "unit_x_arg"),
    ({env_var, x_arg}, "unit_x_arg"),
]

for ways, expected_unit in test_scenarios:
    ways_str = "-".join(sorted(ways)) or "default"

    wd = Wdir(f"tmp_{ways_str}")

    # Build the test program and produce a trace
    gpr = gprfor(
        ["main.adb"],
        srcdirs="..",
        extra="""
        GPR_Tool := external ("GPR_TOOL", "");
        package Coverage is
            for Units use ("unit_" & GPR_Tool);
        end Coverage;
    """,
    )

    # Prepare the (sometimes conflicting) ways to convey the GPR_TOOL variable
    # to gnatcov.
    common_xcov_args = []
    os.environ.pop("GPR_TOOL", None)
    if env_var in ways:
        os.environ["GPR_TOOL"] = "env_var"
    if x_arg in ways:
        common_xcov_args.append("-XGPR_TOOL=x_arg")

    # Produce a coverage report for this configuration
    build_run_and_coverage(
        gprsw=GPRswitches(root_project=gpr),
        covlevel="stmt",
        mains=["main"],
        extra_coverage_args=["-axcov"],
        extra_args=common_xcov_args,
    )

    # Check that the report contains what we expect
    reported_units = sorted(glob.glob(os.path.join("obj", "*.xcov")))
    expected_report = os.path.join("obj", f"{expected_unit}.adb.xcov")

    thistest.fail_if(
        reported_units != [expected_report],
        "Unexpected set of coverage reports, expected:\n"
        "   {}\n"
        "but got:\n"
        "{}".format(
            expected_report,
            "\n".join("   {}".format(f) for f in reported_units),
        ),
    )

    wd.to_homedir()

thistest.result()
