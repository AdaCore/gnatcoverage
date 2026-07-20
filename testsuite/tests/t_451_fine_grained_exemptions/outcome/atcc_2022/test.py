"""
Check decision outcome exemptions on assertions with ATC in Ada 2022
(conditions can be instrumented).
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.decision, assert_lvl="atcc").run()
thistest.result()
