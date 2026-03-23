"""
Test that the introduction of witness function call in the decision expressions
does not generate warnings with -Wsign-conversion, on logical and relational
operators.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


# Run the testcases for decision and mcdc to test all witness_* function
# variants.
#
# Enable warnings as errors to stop the test should we get any warnings, as
# otherwise the output of gprbuild is not checked.
TestCase(extracargs="-Wsign-conversion -Werror", category=CAT.decision).run()
TestCase(extracargs="-Wsign-conversion -Werror", category=CAT.mcdc).run()

thistest.result()
