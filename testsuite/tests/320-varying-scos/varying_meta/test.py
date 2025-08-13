"""
Check that gnatcov correctly discards coverage data for a compilation unit
version that conflicts with a previously loaded version.
"""

from SUITE.context import thistest
from SCOV.tctl import CAT
from SCOV.tc import TestCase

tolerate_cov_messages = "|".join(
    [
        r"warning: Discarding source coverage data for unit"
        r".*foo.h \(from .*bar.c\)",
        r"warning: traces for .*foo.h .* are inconsistent .*",
    ]
)
TestCase(
    category=CAT.stmt,
    tolerate_cov_messages=tolerate_cov_messages,
).run()

thistest.result()
