"""
Test that gnatcov reports decisions for which no conditional branches have been
generated, and which does not dominate any statement as not-coverable instead
of a violation.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT, CovControl
from SUITE.context import thistest


TestCase(category=CAT.decision).run()

if thistest.options.trace_mode == "bin":
    TestCase(category=CAT.decision).run(
        covcontrol=CovControl(covoptions="--non-coverable"), subdirhint="nc_"
    )

thistest.result()
