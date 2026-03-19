# Regression test: MCDC instrumentation for contracts used to insert MCDC state
# in a "too global" scope, resulting in the corruption of coverage buffers
# (wrong MCDC path index computed).

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.mcdc, assert_lvl="atcc").run()
thistest.result()
