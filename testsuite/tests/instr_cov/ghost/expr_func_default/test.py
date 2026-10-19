from SCOV.tc import TestCase
from SUITE.context import thistest
from SCOV.tctl import CAT


TestCase(category=CAT.stmt).run()
TestCase(category=CAT.stmt, instrument_ghost=True).run()
thistest.result()
