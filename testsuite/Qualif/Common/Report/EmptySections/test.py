from SCOV.tc import *
from SCOV.report import ReportChecker

tc = TestCase(category="stmt")
tc.run()

ReportChecker(
    "test_sort_full", ntraces=1, xcovlevel=tc.xcovlevels[0], xregions=0
    ).run()

thistest.result()
