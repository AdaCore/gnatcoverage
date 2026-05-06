from SCOV.tc import TestCase
from SUITE.context import thistest
from SCOV.tctl import CAT

TestCase(category=CAT.mcdc).run()
TestCase(category=CAT.mcdc, instrument_ghost=True).run()
thistest.result()
