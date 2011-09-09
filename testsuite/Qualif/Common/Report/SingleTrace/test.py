from SCOV.tc import *
from SCOV.report import ReportChecker

category="stmt"

tc=TestCase(category=category)
tc.run()

ReportChecker(
    "test_sort_lt", ntraces=1, xcovlevel=tc.xcovlevels[0], xregions=0
    ).run()

thistest.result()
