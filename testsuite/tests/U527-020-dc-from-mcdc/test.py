"""
Test that stmt+decision analysis is allowed and functional
on top of stmt+mcdc instrumentation.
"""

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import xcov, gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir('tmp_')

gpr = gprfor(mains=["main.adb"], srcdirs=[".."])

# Instrument, build and produce a trace for stmt+mcdc on a very simple
# program. Retrieve the base arguments that we would need to pass to gnatcov
# coverage for an analysis of the same SCOs at that --level:

xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=gpr),
    extra_coverage_args=[],
    covlevel='stmt+mcdc',
    mains=['main'],
    trace_mode='src')

# Switch to --level=stmt+decision, add the arguments for the
# output format/location then check that we can produce a report:

xcov_args = [
    '--level=stmt+decision' if arg.startswith('--level') else arg
    for arg in xcov_args]

xcov_args.extend(['--annotate=xcov', '--output-dir=xcov'])

xcov(xcov_args)

check_xcov_reports('xcov', {'main.adb.xcov': {'+': {2}, '!': {5}, '-': {6}}})

thistest.result()
