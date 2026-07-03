"""
Check coverage reports for an assertion when its outcome True is exempted.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.decision, assert_lvl="atc").run()
thistest.result()
