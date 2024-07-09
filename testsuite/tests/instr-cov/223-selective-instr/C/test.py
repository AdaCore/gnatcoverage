"""
Check gnatcov support for selective instrumentation for C when a file has
various preprocessing configuration resulting in varying preprocessed versions,
and the user has disabled coverage for every code region differing across
preprocessing invocations.

Note that in this case, the user must use checkpoint consolidation, as there
can only exists one version of the SIDs at a time.
"""

from SUITE.context import thistest
from SCOV.tctl import CAT
from SCOV.tc import TestCase

thistest.options.consolidate = "checkpoints"
TestCase(
    category=CAT.mcdc,
    tolerate_messages=(
        r".* No justification given for disabled coverage region"
    ),
).run()

thistest.result()
