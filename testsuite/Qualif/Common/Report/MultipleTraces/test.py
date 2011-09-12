from SCOV.tc import *
from SCOV.report import ReportChecker

tc=TestCase(category=None)
tc.run()

ReportChecker(
    "cons_df", ntraces=2, xcovlevel=tc.xcovlevels[0], xregions=0
    ).run()

thistest.result()
