"""
Sanity test to ensure gnatcov does not crash when instrumenting code
containing a default subtype mark for a generic formal type.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.stmt).run()
thistest.result()
