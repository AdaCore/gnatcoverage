from SCOV.tc import TestCase
from SUITE.context import thistest
from SCOV.tctl import CAT

TestCase(category=CAT.mcdc).run()
thistest.result()
