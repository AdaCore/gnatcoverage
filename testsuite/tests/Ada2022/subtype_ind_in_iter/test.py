"""
Test the correct handling of subtype indications in generalized loop
iterations. As the subtype indication in the loop must statically match the
type of the iterated cursor type, we can only create static decisions that are
not currently instrumented by gnatcov. This test thus only really checks that
there is no crash in the presence of this construct.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.mcdc).run()
thistest.result()
