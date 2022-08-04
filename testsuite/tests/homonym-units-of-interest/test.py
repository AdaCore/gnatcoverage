"""
This test checks that gnatcov can deal with two homonym files belonging to
different projects, with one of the source files being excluded from the
coverage analysis (through project file coverage attributes) while the other
is not.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir
from SUITE.tutils import thistest, gprfor

Wdir('tmp_')

is_bin_traces = thistest.options.trace_mode == 'bin'

extra_prj1 = """
package Coverage is
   for Excluded_Units use ("pkg.c");
end Coverage;
"""
prj1 = gprfor(mains=[], prjid='prj1', srcdirs=['../prj1'], objdir='obj-prj1',
              extra=extra_prj1)
prj2 = gprfor(mains=[], prjid='prj2', srcdirs=['../prj2'], objdir='obj-prj2')
p = gprfor(mains=['test.c'], deps=['prj1', 'prj2'], srcdirs=['..'])

build_run_and_coverage(
    gprsw=GPRswitches(root_project=p),
    covlevel='stmt+mcdc',
    mains=['test'],
    extra_coverage_args=['--annotate=xcov', '--output-dir=xcov'],
    instrument_warnings_as_errors=False,
)

# The filtering of units of interest is done at a later stage for binary
# traces, and files that may be ignored in coverage report can have been
# added to gnatcov file table. This means that the shortest unique suffix,
# computed from gnatcov file table to differentiate homonym source files
# will be different from source traces.
xcov_homonoym_filename = 'prj2-pkg.c.xcov' if is_bin_traces else 'pkg.c.xcov'
check_xcov_reports(
    '*.xcov',
    {
        xcov_homonoym_filename: {'!': {4}},
        'test.c.xcov': {'+': {7, 8, 9}}
    },
    'xcov'
)

thistest.result()
