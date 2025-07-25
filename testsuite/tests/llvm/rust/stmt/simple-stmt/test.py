"""
Ensure a simple 1 statement function is correctly covered.
"""

from SCOV.tc import TestCase
from SUITE.context import thistest

TestCase().run()
thistest.result()
