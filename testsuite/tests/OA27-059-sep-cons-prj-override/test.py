"""
Using similar material as for OA27-059-separate-consolidation, test that the
Coverage'Excluded_Source_Files project attribute is overidden by the
--excluded-source-files command line argument.
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

# The goal of this testcase is to check that the --excluded-source-files
# command-line argument, when provided, takes precedence over the
# Coverage'Excluded_Source_Files project attribute.
#
# In pkg_under_test.gpr, that attribute is defined so that
# pkg_under_test-some_procedure.adb is ignored. To check that
# --excluded-source-files take precedence, we pass it to ignore only
# pkg_under_test-pkg_test.adb, so that pkg_under_test-some_procedure.adb
# should show up in coverage reports.
overriding_ignored_source_file = "pkg_under_test-pkg_test.adb"

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
        ignored_source_files=[overriding_ignored_source_file],
        gprsw_for_coverage=gprsw_for_cov,
        scos_for_run=False,
    )
    trace_files.append(xcov_args.pop())

# This testcase builds extending projects and then runs gnatcov on the extended
# project. This requires the extended project to have its own ALI/SID files, so
# build this project (in binary trace mode, to get ALI files) or instrument it
# (in source trace mode, to get SID files). Since instrumentation uses the
# "ignore source files" information, we also need to pass
# --excluded-source-files here.
if thistest.options.trace_mode == "bin":
    gprbuild(gprsw_for_cov.root_project)
else:
    xcov_instrument(
        gprsw=gprsw_for_cov,
        covlevel=None,
        extra_args=[
            f"--excluded-source-files={overriding_ignored_source_file}"
        ],
    )

# The presence of pkg_under_test-some_procedure.adb and the absence of
# pkg_under_test-pkg_test.adb prove that --excluded-source-files took
# precedence over the Coverage'Excluded_Source_Files project attribute.
clean_output_directory()
p = checked_xcov(xcov_args + ["--output-dir=output"] + trace_files, "cons.log")
check_xcov_reports(
    "output",
    {
        "pkg_under_test.ads.xcov": {},
        "pkg_under_test.adb.xcov": {"+": {7, 8, 10}},
        "pkg_under_test-some_procedure.adb.xcov": {"-": {5}},
    },
)

thistest.result()
