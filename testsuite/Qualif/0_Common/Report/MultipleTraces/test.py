from SCOV.tc import *
from SCOV.report import ReportChecker

TestCase(category="stmt").run()
ReportChecker("cons_sort_gtin",ntraces=2).run()
thistest.result()
