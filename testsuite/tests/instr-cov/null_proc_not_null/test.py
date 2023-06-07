"""
Check that the instrumentation of null procedures with a "not null" argument
produces valid code.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.stmt).run()
thistest.result()
