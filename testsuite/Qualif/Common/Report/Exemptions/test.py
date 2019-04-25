from SCOV.report import ReportChecker
from SCOV.tc import TestCase
from SUITE.context import thistest

tc = TestCase(category=None)
tc.run()
ReportChecker(tc).run()
thistest.result()
