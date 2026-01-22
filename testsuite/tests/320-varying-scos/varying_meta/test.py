"""
Check that gnatcov correctly discards coverage data for a compilation unit
version that conflicts with a previously loaded version. Note: the use of the
TestCase driver framework matters here, as if we do not discard source coverage
obligations (those coming from bar.h, or foo.h: it does not really matter
which), we end up with four coverage obligations in foo.h, and thus four
violations. This is actually checked through the report format check by the
TestCase framework.
"""

from SUITE.context import thistest
from SUITE.cutils import contents_of
from SCOV.tctl import CAT
from SCOV.tc import TestCase

tolerate_cov_messages = "|".join(
    [
        r"warning: Discarding source coverage data for unit .*foo.h \(from"
        r"(.*bar.c\), loaded from .*foo.c.sid"
        r"|.*foo.c\), loaded from .*bar.c.sid)",
        r"warning: traces for .*foo.h .* are inconsistent .*",
    ]
)
TestCase(
    category=CAT.stmt,
    tolerate_cov_messages=tolerate_cov_messages,
).run()

# Check that gnatcov output contains the expected error messages
thistest.fail_if_no_match(
    "gnatcov coverage output",
    tolerate_cov_messages,
    contents_of("s_1/xcov.out").strip(),
)

thistest.result()
