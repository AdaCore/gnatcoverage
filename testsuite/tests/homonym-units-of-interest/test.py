"""
This test checks that gnatcov can deal with two homonym files belonging to
different projects, with one of the source files being excluded from the
coverage analysis (through project file coverage attributes) while the other
is not.
"""

import os.path

from e3.fs import sync_tree

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir
from SUITE.tutils import thistest, gprfor, xcov

wd = Wdir('tmp_')

# Run the build and run in a dedicated directory

prj1 = gprfor(mains=[], prjid='prj1', srcdirs=['../prj1'], objdir='obj-prj1')
prj2 = gprfor(mains=[], prjid='prj2', srcdirs=['../prj2'], objdir='obj-prj2')
p = gprfor(mains=['test.c'], deps=['prj1', 'prj2'], srcdirs=['..'])

xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=p),
    covlevel='stmt+mcdc',
    mains=['test'],
    extra_coverage_args=['--annotate=xcov', '--output-dir=xcov'],
)

# Now onto coverage. Try to ignore pkg.c in prj1, and in prj2 in two different
# gnatcov coverage invocations, using the project attributes as this can't be
# specified unambiguously on the command line.
#
# This checks two things:
#
#   * a user can ignore a specific source file even if it has an homonym in
#     the project tree. Note that the user would probably have the same
#     project for instrument / coverage, but for testing purposes, we also
#     need to check the second point.
#
#   * gnatcov instrument does not generate a single SID file for the two
#     homonym source files.

ignore_pkg = """
package Coverage is
   for Excluded_Units use ("pkg.c");
end Coverage;
"""

wd.to_homedir()


def check_report(prj_ignore, prj_of_interest):
    """
    Check that the report is correct when the homonym source file (pkg.c) is
    ignored in prj_ignore and of interest in prj_of_interest.
    """
    subdir = f'tmp_cov_ignore_{prj_ignore}'
    wd = Wdir(subdir)
    sync_tree(os.path.join('..', 'tmp_'), '.')
    gprfor(mains=[], prjid=prj_ignore, srcdirs=[f'../{prj_ignore}'],
           objdir=f'obj-{prj_ignore}', extra=ignore_pkg)
    xcov(xcov_args)

    # The filtering of units of interest is done at a later stage for binary
    # traces, and files that may be ignored in the coverage report can have
    # been added to gnatcov's file table. This means that the shortest unique
    # suffix computed from gnatcov file table to differentiate homonym source
    # files will be different from source traces.
    xcov_homonoym_filename = (
        f'{prj_of_interest}-pkg.c.xcov' if thistest.options.trace_mode == 'bin'
        else 'pkg.c.xcov'
    )
    check_xcov_reports(
        '*.xcov',
        {
            xcov_homonoym_filename: {'!': {4}},
            'test.c.xcov': {'+': {7, 8, 9}}
        },
        'xcov'
    )
    wd.to_homedir()


check_report('prj1', 'prj2')
check_report('prj2', 'prj1')
thistest.result()
