"""
Test that gnatcov does not generate conflicting entities between a package spec
and its separate body. This could happen if the package is a nested package
declared in a body, and the nested package's body is a separate, in which the
suffixes for the entities gnatcov inserts were not different. The instrumented
code used to not compile at all.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT

from SUITE.context import thistest

TestCase(category=CAT.stmt).run()
thistest.result()
