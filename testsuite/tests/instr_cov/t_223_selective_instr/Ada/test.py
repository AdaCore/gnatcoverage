"""
Check gnatcov support for selective instrumentation for Ada.
"""

from SUITE.context import thistest
from SCOV.tctl import CAT
from SCOV.tc import TestCase

TestCase(
    category=CAT.mcdc,
    tolerate_messages=(
        r".* No justification given for disabled coverage region"
    ),
).run()

thistest.result()
