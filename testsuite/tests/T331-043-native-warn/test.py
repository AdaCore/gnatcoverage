"""Check that "gnatcov run" warns when running a native program."""

import os

from e3.env import Env

from SCOV.minicheck import build_run_and_coverage
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest


tmp = Wdir('tmp')


# The testsuite automatically disables the warning we want to test: override
# this decision just for this testcase.
os.environ.pop('GNATCOV_NO_NATIVE_WARNING', None)

is_native = not Env().is_cross
build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=['..'], mains=['main.adb'])),
    covlevel='stmt',
    mains=['main'],
    extra_coverage_args=["-axcov"])


warning_exceprt = (
    'Support for coverage of non-instrumented native programs is deprecated'
)


def check_warning_absent(logfile):
    thistest.fail_if(warning_exceprt in contents_of(logfile),
                     "unexpected warning in {}".format(logfile))


def check_warning_present(logfile):
    thistest.fail_if(warning_exceprt not in contents_of(logfile),
                     "missing warning in {}".format(logfile))


# Depending on the trace mode and the target, check the presence/absence of the
# warning.
if thistest.options.trace_mode == 'src':
    check_warning_absent('instrument.log')
elif is_native:
    check_warning_present('main_output.txt')
else:
    check_warning_absent('main_output.txt')
check_warning_absent('coverage.log')

thistest.result()
