"""
Using similar material as for OA27-059-separate-consolidation, test that the
Coverage'Excluded_Source_Files project attribute is handled correctly.
"""

import os.path

from e3.fs import mkdir, rm, sync_tree

from SCOV.minicheck import build_and_run, checked_xcov, check_xcov_reports
from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir
from SUITE.tutils import gprbuild


wd = Wdir("tmp_")

# Copy project sources in the temporary director
for f in [
    "pkg_under_test.gpr",
    "src_under_test",
    "test1",
    "test1.gpr",
    "test2",
    "test2.gpr",
]:
    sync_tree(os.path.join("..", f), f)


class Testcase(object):
    def __init__(self, name, objdir):
        self.name = name
        self._objdir = objdir

    @property
    def project_file(self):
        return f"{self.name}.gpr"

    @property
    def main(self):
        return f"main_{self.name}"

    def obj_dir(self, *args):
        return os.path.join(self._objdir, *args)

    def exe_dir(self, *args):
        return os.path.join("bin", *args)


def clean_output_directory():
    rm("output")
    mkdir("output")


test1 = Testcase("test1", "obj1")
test2 = Testcase("test2", "obj2")
testcases = [test1, test2]
gprsw_for_cov = GPRswitches(root_project="pkg_under_test.gpr")

# Build the test material: program and traces
trace_files = []
for testcase in testcases:
    xcov_args = build_and_run(
        gprsw=GPRswitches(root_project=testcase.project_file),
        covlevel=None,
        mains=[testcase.main],
        gpr_obj_dir=testcase.obj_dir(),
        gpr_exe_dir=testcase.exe_dir(),
        extra_coverage_args=[],
        gprsw_for_coverage=gprsw_for_cov,
        scos_for_run=False,
    )
    trace_files.append(xcov_args.pop())

# This testcase builds extending projects and then runs gnatcov on the extended
# project. This requires the extended project to have its own ALI/SID files, so
# build this project (in binary trace mode, to get ALI files) or instrument it
# (in source trace mode, to get SID files).
if thistest.options.trace_mode == "bin":
    gprbuild(gprsw_for_cov.root_project)
else:
    xcov_instrument(gprsw=gprsw_for_cov, covlevel=None)

# Check that the Excluded_Source_Files project attribute makes gnatcov ignore
# the conflicting symbols between test1 and test2, and succeeds to create a
# report with the expected coverage data.
clean_output_directory()
p = checked_xcov(xcov_args + ["--output-dir=output"] + trace_files, "cons.log")
check_xcov_reports(
    "output",
    {
        "pkg_under_test.ads.xcov": {},
        "pkg_under_test.adb.xcov": {"+": {7, 8, 10}},
    },
)

thistest.result()
