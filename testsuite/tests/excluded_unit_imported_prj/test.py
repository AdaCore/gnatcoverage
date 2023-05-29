"""
This test checks that units of interest project attributes only apply to the
project in which they are defined, and not to recursive project dependencies.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.gprutils import GPRswitches
from SUITE.cutils import contents_of, Wdir
from SUITE.tutils import thistest, gprfor

from e3.fs import mkdir

Wdir('tmp_')

prj1 = gprfor(mains=[], prjid='prj1', srcdirs=['../prj1'], objdir='obj-prj1')

# Prevent warnings about inexistant obj dir, that some versions of gnatcov
# might emit when instrumenting before building:
mkdir("obj-prj1")

# Check that ignoring pkg.c in the main project yields a warning, as the pkg.c
# unit is in thr prj1 dependency and not in the root project.
extra_p = """
package Coverage is
   for Excluded_Units use ("pkg.c");
end Coverage;
"""
p = gprfor(mains=['test.c'], deps=['prj1'], srcdirs=['..'], extra=extra_p)

build_run_and_coverage(
    gprsw=GPRswitches(root_project=p),
    covlevel='stmt+mcdc',
    mains=['test'],
    extra_coverage_args=['--annotate=xcov'],
    instrument_warnings_as_errors=False,
)
warning_file = ('coverage.log' if thistest.options.trace_mode == 'bin'
                else 'instrument.log')

thistest.fail_if_not_equal(
    warning_file,
    'warning: no unit pkg.c in project gen'
    ' (coverage.excluded_units attribute)\n',
    contents_of(warning_file)
)

# Check that pkg.c is not ignored by checking the report contents
check_xcov_reports(
    '*.xcov',
    {
        'pkg.c.xcov': {'!': {4}},
        'test.c.xcov': {'+': {6, 7}}
    },
    'obj'
)

thistest.result()
