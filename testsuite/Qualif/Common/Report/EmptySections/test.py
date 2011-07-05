from SCOV.tc import *
from SCOV.report import ReportChecker

TestCase(category="stmt").run()
ReportChecker("test_sort_full",ntraces=1).run()
thistest.result()
