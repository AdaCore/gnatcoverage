"""
Check coverage behavior for a simple code block
"""

from SCOV.tc import TestCase
from SUITE.context import thistest

TestCase().run()
thistest.result()
