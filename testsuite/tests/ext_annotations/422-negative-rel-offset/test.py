"""
Check that gnatcov add-annotation does not crash when the designated region
has columns that are smaller than the starting column of the enclosing named
declaration. This used to make stable_sloc (and thus gnatcov) crash.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CovControl, CAT
from SUITE.context import thistest
from SUITE.tutils import Exempt_Region, generate_annotations

annotations = [
    Exempt_Region("src/pkg.adb", "8:1", "9:30", "Unreachable"),
]

annot_file = generate_annotations(annotations)

TestCase(category=CAT.stmt).run(
    covcontrol=CovControl(covoptions=f"--external-annotations={annot_file}")
)
thistest.result()
