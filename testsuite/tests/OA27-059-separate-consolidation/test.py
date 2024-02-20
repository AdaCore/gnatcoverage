"""
Test that we have no consolidation error in source coverage for two binaries
that bring different versions of the same separate procedure. The separate
procedure is part of an unit of interest, but the coverage campaign ignores the
separate itself.
"""

import os
import os.path
import shutil

from SCOV.minicheck import build_and_run, checked_xcov, check_xcov_reports
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir
from SUITE.tutils import xcov


# Clean up projects build artifacts
for obj_dir in ('bin', 'obj1', 'obj2'):
    if os.path.exists(obj_dir):
        shutil.rmtree(obj_dir)

wd = Wdir('tmp_')


class Testcase(object):
    def __init__(self, name, objdir):
        self.name = name
        self._objdir = objdir

    @property
    def project_file(self):
        return os.path.join('..', '{}.gpr'.format(self.name))

    @property
    def main(self):
        return 'main_{}'.format(self.name)

    def obj_dir(self, *args):
        return os.path.join('..', self._objdir, *args)

    def exe_dir(self, *args):
        return os.path.join('..', 'bin', *args)


def clean_output_directory():
    if os.path.exists('output'):
        shutil.rmtree('output')
    os.mkdir('output')


test1 = Testcase('test1', 'obj1')
test2 = Testcase('test2', 'obj2')
testcases = [test1, test2]


def build_and_run_tests(ignored_source_files=[]):
    """
    Build the test material: program and traces.
    """
    trace_files = []
    for testcase in testcases:
        xcov_args = build_and_run(
            gprsw=GPRswitches(root_project=testcase.project_file,
                              units=['pkg_under_test']),
            covlevel='stmt+decision',
            mains=[testcase.main],
            ignored_source_files=ignored_source_files,
            gpr_obj_dir=testcase.obj_dir(),
            gpr_exe_dir=testcase.exe_dir(),
            extra_coverage_args=['--annotate=xcov', '--output-dir=output'],
            scos_for_run=False)
        trace_files.append(xcov_args.pop())

    return xcov_args + trace_files


# Build and run tests without ignored source files. Check that we indeed have a
# consolidation error by default.
clean_output_directory()
xcov_args = build_and_run_tests()
p = xcov(xcov_args, out='cons-1.log', register_failure=False)
thistest.fail_if(
    p.status == 0,
    '"gnatcov coverage" is supposed to complain about different symbols during'
    ' consolidation, but it did not.'
)

# Build and run tests with ignored source files. Check that the new option
# makes it ignore the problematic symbols, and succeeds to create a report with
# the expected coverage data.
clean_output_directory()
xcov_args = build_and_run_tests(['pkg_under_test-pkg_test.adb'])
p = checked_xcov(xcov_args, 'cons-2.log')
check_xcov_reports('output', {'pkg_under_test.adb.xcov': {'+': {7, 8, 10}}})

thistest.result()
