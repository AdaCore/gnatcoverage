from SCOV.tc import TestCase
from SCOV.htc import CAT
from SUITE.context import thistest


TestCase(category=CAT.decision).run()
TestCase(category=CAT.mcdc).run()
thistest.result()
