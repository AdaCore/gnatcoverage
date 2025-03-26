"""
Check gnatcov support for selective instrumentation for C when a file has
various preprocessing configuration resulting in varying preprocessed versions,
and the user has disabled coverage for every code region differing across
preprocessing invocations.

Note that in this case, the user must use checkpoint consolidation, as there
can only exists one version of the SIDs at a time.

This version of the test uses external annotations instead of comments, which
are always loaded irrespectively of preprocessing configuration, we thus have
more regions flagged as "disabled coverage"
"""

from SUITE.context import thistest
from SCOV.tctl import CAT, CovControl
from SCOV.tc import TestCase

from SUITE.tutils import Cov_Off, Cov_On, generate_annotations

# Annotations to be used, use a mix of Exempt_Region an Exempt_On/Off
annotations = generate_annotations(
    [
        Cov_Off("src/pkg.h", "5:3", None),
        Cov_On("src/pkg.h", "7:3", None),
        Cov_Off("src/pkg.h", "11:3", None),
        Cov_On("src/pkg.h", "13:3", None),
    ],
    tolerate_messages="--justification missing",
)

thistest.options.consolidate = "checkpoints"
TestCase(
    category=CAT.mcdc,
    tolerate_messages=(
        r".* Missing or empty justification for external disabled coverage"
        r' region annotation ".*"'
    ),
).run(CovControl(instroptions=f"--external-annotations={annotations}"))

thistest.result()
