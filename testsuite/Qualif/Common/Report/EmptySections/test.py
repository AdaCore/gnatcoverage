from SCOV.tc import *
from SCOV.report import ReportChecker

tc=TestCase(category=None)
tc.run()

ReportChecker(
    "test_expr_full", ntraces=1, xcovlevel=tc.xcovlevels[0], xregions=0
    ).run()

thistest.result()
