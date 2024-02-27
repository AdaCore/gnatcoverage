"""
Check that gnatcov correctly instruments an extending project when a source
includes a header belonging to the extended project. Also check that it picks
the version of the header that is in the ultimate extending project.
"""

import os

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir
from SUITE.tutils import thistest

Wdir("tmp_")


def process(gpr_dir, expected_cov):
    gpr_obj_dir = os.path.join(gpr_dir, "obj")
    build_run_and_coverage(
        gprsw=GPRswitches(root_project=os.path.join(gpr_dir, "prj.gpr")),
        covlevel="stmt",
        mains=["test"],
        gpr_obj_dir=gpr_obj_dir,
        gpr_exe_dir=gpr_obj_dir,
        extra_coverage_args=["--annotate=xcov"],
    )
    check_xcov_reports(gpr_obj_dir, expected_cov)


# Check that the header in the extended project is picked by gnatcov instrument
# when there is no version in the extending project.
expected_cov = {
    "test.c.xcov": {"+": {6}},
    "support.h.xcov": {"+": {4}},
}
process(os.path.join("..", "include_base_foo"), expected_cov)

# If there is a version of the header in the extending project, check that this
# is the one picked.
expected_cov = {
    "test.c.xcov": {"+": {6}},
    "support.h.xcov": {"+": {6}},
}
process(os.path.join("..", "include_foo"), expected_cov)

thistest.result()
