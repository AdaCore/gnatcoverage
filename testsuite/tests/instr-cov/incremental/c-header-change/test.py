"""
Check that gnatcov only reinstruments the modified source.
"""

from e3.fs import cp

from SCOV.minicheck import (
    build_and_run,
    build_run_and_coverage,
    check_xcov_reports,
)
from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

# Copy the sources as we will modify them
cp("../pkg.h", ".")
cp("../pkg.c", ".")
cp("../main.c", ".")

# Create a project
root_prj = gprfor(srcdirs=["."], mains=["main.c"], langs=["C"])

# The first instrumentation command should instrument all of the files
build_and_run(
    gprsw=GPRswitches(root_project=root_prj),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=[],
)

# Make a header modification
with open("pkg.h", "a") as f:
    f.write("\n")

# Check that gnatcov reinstruments pkg.c, AND test.c
env.add_search_path("ADA_DEBUG_FILE", "../../.gnatdebug")
build_run_and_coverage(
    gprsw=GPRswitches(root_project=root_prj),
    covlevel="stmt",
    mains=["main"],
    extra_coverage_args=["-axcov"],
)

# Check incrementality results
thistest.fail_if_diff(
    baseline_file="../instrument.expected",
    actual_file="instrument.log",
)

# Check coverage report
check_xcov_reports(
    "obj", {"pkg.c.xcov": {"+": {6}}, "main.c.xcov": {"+": {6, 7}}}
)
thistest.result()
