from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.decision, assert_lvl="atc").run()
TestCase(category=CAT.decision, assert_lvl="atcc").run()
thistest.result()
