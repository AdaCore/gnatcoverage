"""
Basic test to ensure exemptions are properly imported by gnatcov
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT, CovControl
from SUITE.context import thistest

# Run the same test but passing the annotations either at instrumentation time
# or at coverage time. This should yield the same results.

TestCase(category=CAT.decision).run(
    CovControl(
        covoptions="--external-annotations=../exemptions.toml",
        instroptions="",
    ),
    subdirhint="cov_",
)

if thistest.options.trace_mode == "src":
    TestCase(category=CAT.decision).run(
        CovControl(
            instroptions="--external-annotations=../exemptions.toml",
        ),
        subdirhint="instr_",
    )

thistest.result()
