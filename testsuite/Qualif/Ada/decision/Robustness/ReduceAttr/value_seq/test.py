"""
Test checking that gnatcov correctly processes sources containing reduce
attributes, with a value sequence prefix, and that decisions withing the value
sequence are correctly instrumented.
"""

from SCOV.tc import TestCase
from SUITE.context import thistest

TestCase().run()
thistest.result()
