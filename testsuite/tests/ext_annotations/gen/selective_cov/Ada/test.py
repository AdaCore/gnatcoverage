"""
Check gnatcov support for selective instrumentation for Ada.

This variation of the test uses external annotations instead of
in-source pragmas.
"""

from SUITE.context import thistest
from SCOV.tctl import CAT, CovControl
from SCOV.tc import TestCase

TestCase(
    category=CAT.mcdc,
    tolerate_messages=(
        r".* Missing or empty justification for external disabled coverage"
        r' region annotation ".*"'
    ),
).run(CovControl(instroptions="--external-annotations=../annotations.toml"))

thistest.result()
