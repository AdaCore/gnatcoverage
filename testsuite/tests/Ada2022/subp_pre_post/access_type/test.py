from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.mcdc, assert_lvl="atcc").run()
thistest.result()
