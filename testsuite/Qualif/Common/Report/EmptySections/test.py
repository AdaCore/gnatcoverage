from SCOV.tc import *
from SCOV.report import ReportChecker

category="stmt"
TestCase(category=category).run()
ReportChecker("test_sort_full", ntraces=1, category=category).run()
thistest.result()
