"""
Verify the correct behavior of the unit selection facilities for C.

We perform basic tests here and assume that more sophisticated will work as
well as they all build up on top of the same internal circuits.

We have one functional unit and one test driver unit, with a template project
file from which we can build, then analyse. We perform different analysis
attempts, all intended to produce results for the functional unit only. This is
achieved by either exluding the harness unit out of the default, leaving only
the functional unit, or by explicitly including the functional unit only.
"""

import re

from e3.fs import mkdir, ls

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


Wdir('tmp_')


def gengpr(attributes, prjname):
    return gprfor(
        mains=["test_abs.c"],
        prjid=prjname,
        srcdirs='..',
        objdir='obj',
        exedir='.',
        langs=['C'],
        extra="""
            package Coverage is
                {}
            end Coverage;
        """.format('\n'.join(attributes))
    )


# Build and run first
gprname = gengpr(attributes=[], prjname='build')
xcov_args = build_and_run(GPRswitches(gprname), "stmt", ["test_abs"],
                          extra_coverage_args=["--annotate=xcov"])

# Now analyse with different unit selection schemes ...  Assign a different
# directory to each attempt. Expect reports for the functional unit(s) only.


def trycov(attributes, outdir):
    # Create dedicated dir and analyse with the specific attributes:
    mkdir(outdir)
    gprname = gengpr(attributes=attributes, prjname=outdir)
    xcov('%s -P%s --output-dir=%s' % (' '.join(xcov_args), gprname, outdir))

    # Check that we have no report for the harness unit and that we have
    # a report which looks correct for the functional unit:
    harness_reports = ls('%s/test*.xcov' % outdir)
    thistest.fail_if(harness_reports,
                     'unexpected coverage reports for harness units: %s'
                     % str(harness_reports))

    all_reports = ls('%s/*.xcov' % outdir)
    thistest.fail_if(
        not all_reports,
        'missing expected coverage reports for !harness units in %s' % outdir)

    # We expect to find a mix of coverage notes in each expected
    # report:

    def check_report(r):
        contents = contents_of(r)

        for note in ('+', '-', '.'):
            notes = re.findall(r'\d+ \%c:' % note, contents)
            thistest.fail_if(
                not notes, "missing expected '%c' notes in %s" % (note, r))

    for r in all_reports:
        check_report(r)


trycov(attributes=['for excluded_units use ("test_abs.c");'],
       outdir='exclude_harness')
trycov(attributes=['for units use ("abs.c");'],
       outdir='include_functional')

thistest.result()
