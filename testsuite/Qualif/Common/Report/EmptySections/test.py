from SCOV.tc import *
from SCOV.report import ReportChecker

tc=TestCase(category=None)
tc.run()

ReportChecker(tc).run()

thistest.result()
