"""
Check coverage reports with a branch exemption in the "THEN" part of an Ada IF
statement.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.decision).run()
thistest.result()
