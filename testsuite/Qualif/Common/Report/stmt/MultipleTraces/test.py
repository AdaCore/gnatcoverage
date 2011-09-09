from SCOV.tc import *
from SCOV.report import ReportChecker

tc = TestCase()
tc.run()

ReportChecker(
    "cons_sort_gtin", ntraces=2, xcovlevel=tc.xcovlevels[0], xregions=0
    ).run()

thistest.result()
