"""
Check that gnatcov correctly cleans any previously existing xcov report before
producing a new one.
"""

from SCOV.minicheck import build_and_run, xcov, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

from e3.fs import mkdir

import os

tmp = Wdir("tmp_")

expected_cov = {
    "main.adb": {"+": {4, 7}},
    "pkg.adb": {"+": {5}},
    "pkg.ads": {},
}

possible_output_dirs = ["obj", os.path.join("obj", "xcov")]


def check_one(units, output_dir, xcov_args):
    """
    Run gnatcov coverage with the specified xcov_args and check that we only
    get a coverage report for units in output_dir. Also check that all
    directories that are not output_dir contain no .xcov files.
    """
    xcov(xcov_args)
    for dir in possible_output_dirs:
        thistest.log(f"inspecting {dir}")
        check_xcov_reports(
            dir,
            {f"{unit}.xcov": expected_cov[unit] for unit in units}
            if dir == output_dir
            else {},
        )


# Create the traces
xcov_args = build_and_run(
    gprsw=GPRswitches(gprfor(mains=["main.adb"], srcdirs=[".."])),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=[],
)

# Create the possible output dirs
for dir in possible_output_dirs:
    mkdir(os.path.join(os.getcwd(), dir))

# First, produce a report for all units, with a single report format specified
thistest.log("=== Step 1: all units in obj ===")
check_one(["main.adb", "pkg.adb", "pkg.ads"], "obj", xcov_args + ["-axcov"])

# Then only create a report for pkg, still a single report format
thistest.log("=== Step 2: pkg in obj ===")
check_one(["pkg.adb", "pkg.ads"], "obj", xcov_args + ["-axcov", "--units=pkg"])

# Now create a report for both units, but specifying two output formats
# (thus creating a subdir).
thistest.log("=== Step 3: all units in obj/xcov ===")
check_one(
    ["main.adb", "pkg.adb", "pkg.ads"],
    os.path.join("obj", "xcov"),
    xcov_args + ["-axcov,html"],
)

# An finally check that the subdir is also cleaned when once again generating
# report with a single report format.
thistest.log("=== Step 4: pkg in obj ===")
check_one(["pkg.adb", "pkg.ads"], "obj", xcov_args + ["-axcov", "--units=pkg"])

thistest.result()
