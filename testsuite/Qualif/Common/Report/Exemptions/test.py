from SCOV.tc import *
from SCOV.report import ReportChecker

category="stmt"
TestCase(category=category).run()
ReportChecker("test_ranges_invalid", ntraces=1, category=category,
              xregions=1).run()
ReportChecker("test_ranges_overlap", ntraces=1, category=category,
              xregions=1).run()
thistest.result()

