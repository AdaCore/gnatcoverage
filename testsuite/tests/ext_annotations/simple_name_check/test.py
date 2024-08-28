"""
Test that attempting to retrieve external exemptions for a file with
no obligations does not crash on binary traces. This used to happen
because such files were not registered under their full name in the
file table, thus resulting in a crash when attempting to get their
full name.

In this instance, gnatcov would crash attempting to retrieve the full filename
for reset.ads.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT, CovControl
from SUITE.context import thistest

TestCase(category=CAT.stmt).run(
    CovControl(
        covoptions="--external-annotations=../annotations.toml",
        instroptions="",
    ),
)

thistest.result()
