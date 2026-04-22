"""
Check that when stmt+decision is enabled, Invariant and Type_Invariant aspects
expressions are not instrumented.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

TestCase(category=CAT.decision).run()
thistest.result()
