from SCOV.tc import *
from SCOV.report import ReportChecker

category="stmt"
TestCase(category=category).run()
ReportChecker("test_sort_lt", ntraces=1, category=category, xregions=0).run()
thistest.result()
