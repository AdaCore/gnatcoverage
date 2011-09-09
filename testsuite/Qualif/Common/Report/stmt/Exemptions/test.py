from SCOV.tc import *
from SCOV.report import ReportChecker

tc=TestCase()
tc.run()

ReportChecker(
    "test_ranges_invalid", ntraces=1, xcovlevel=tc.xcovlevels[0], xregions=1
    ).run()
ReportChecker(
    "test_ranges_overlap", ntraces=1, xcovlevel=tc.xcovlevels[0], xregions=1
    ).run()
thistest.result()

