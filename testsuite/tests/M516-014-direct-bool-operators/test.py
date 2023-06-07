from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.stmt).run()
TestCase(category=CAT.decision).run()
thistest.result()
