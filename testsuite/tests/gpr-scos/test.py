"""
Check the interactions between --scos/--sid and -P regarding the set of units
of interest. Essentially, --scos/--sid is expected to override whatever the -P
family would yield.
"""

from e3.fs import ls, rm

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


wd = Wdir('tmp_')

# Build and run our example test, 1 driver + 2 units
project = gprfor(srcdirs=['../src'], mains=['test_ops.adb'])
xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=project),
    covlevel='stmt',
    mains=['test_ops'],
    extra_coverage_args=['--annotate=xcov'])


# Now exercise various combinations of units of interest
# specifications with --scos and/or -P, and check they produce
# the set of .xcov reports we expect.

# Arrange to always produce the reports in the current directory,
# regardless of --scos or -P, so they are straightforward to find
# or remove in all circumstances.

def trycov(sco_units, with_project, expected_reports):
    rm('*.xcov', recursive=False)

    src_trace = thistest.options.trace_mode == 'src'
    sco_args = [
        '--{}=obj/{}.{}'.format('sid' if src_trace else 'scos',
                                unit,
                                'sid' if src_trace else 'ali')
        for unit in sco_units
    ]
    project_args = ['-P', project] if with_project else []
    xcov(xcov_args + sco_args + project_args + ['--output-dir=.'])

    reports = ls('*.xcov')

    thistest.fail_if(
        set(reports) != set(expected_reports),
        'With %s, %s (found) != %s (expected)' % (
            str(sco_args), str(reports), str(expected_reports)))


# --scos/--sid alone
trycov(sco_units=['inc'], with_project=False,
       expected_reports=['inc.adb.xcov'])

# -P alone
trycov(sco_units=[], with_project=True,
       expected_reports=['inc.adb.xcov',
                         'dec.adb.xcov',
                         'test_ops.adb.xcov'])

# --scos with -P, check that --scos prevails
trycov(sco_units=['dec'], with_project=True,
       expected_reports=['dec.adb.xcov'])

thistest.result()
