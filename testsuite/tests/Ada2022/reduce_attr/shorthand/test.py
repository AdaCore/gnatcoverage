"""
Check that gnatcov correctly processes sources containing reduce attributes, in
their shorthand form (no value sequence), and that nested decisions in the
initial value are correctly instrumented.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

TestCase(category=CAT.decision).run()
thistest.result()
