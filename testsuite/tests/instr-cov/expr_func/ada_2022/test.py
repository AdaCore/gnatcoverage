"""
Check that limitations concerning expression function instrumentation are
lifted when Ada 2022, and thus declare expressions are available.

This test reflects the various limitations that are documented in the user
manual:
- generic expression functions (gen.ads)
- expression functions which are primitives of a tagged type, when that type is
  the return type of the expression function. (prim.ads)
- Recursive expression functions which are a primitive of some type (rec.ads)

The above constructs used to not be instrumented by gnatcov and resulted in
warnings and undetermined coverage items in the reports.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.mcdc).run()
thistest.result()
