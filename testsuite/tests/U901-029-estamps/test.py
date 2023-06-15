"""
Check that the command execution and trace creation dates
advertised in reports look reasonable.
"""

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

import re
from datetime import date

tmp = Wdir('tmp_')

gpr = gprfor(mains=["main.adb"], srcdirs=[".."])

# Build, run, and analyze to produce a "report" output, then verify that the
# dates advertised in the report are consistent with what we perceive is the
# current date.

report = 'report'

xcov_args = build_run_and_coverage(
    gprsw=GPRswitches(root_project=gpr),
    extra_coverage_args=["--annotate=report", "-o", report],
    covlevel='stmt',
    mains=['main'])

# We expect to find two items mentioning the date:
#
#   Date and time of execution: 2021-09-15 20:20:50 +02:00
#
# Then:
#
#   Trace files:
#
#   main.trace
#    ...
#    date     : 2021-09-15 20:20:50 +02:00
#
# Fetch the date components and check:

today = date.today().isoformat()

report = contents_of(report)

date_re = r"\d{4}-\d{2}-\d{2}"

for pattern in (
        r"Date.*execution: (?P<date>%s)" % date_re,
        r"date *: (?P<date>%s)" % date_re
):
    m = re.search(pattern=pattern, string=report)

    thistest.fail_if(
        not m,
        "couldn't find a match for pattern '%s' in report" % pattern)

    thistest.fail_if(
        m.group('date') != today,
        "date found (%s) doesn't match today (%s)" % (m.group('date'), today))

thistest.result()
