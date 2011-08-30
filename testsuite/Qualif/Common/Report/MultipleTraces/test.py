from SCOV.tc import *
from SCOV.report import ReportChecker

category="stmt"
TestCase(category=category).run()
ReportChecker("cons_sort_gtin", ntraces=2, category=category, xregions=0).run()
thistest.result()
