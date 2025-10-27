from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.mcdc, assert_lvl="atc", fun_call_lvl=True).run()
thistest.result()
