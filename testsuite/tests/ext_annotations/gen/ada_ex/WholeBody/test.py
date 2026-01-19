from SCOV.tc import TestCase
from SCOV.tctl import CovControl, CAT
from SUITE.context import thistest

from SUITE.tutils import Exempt_Region, generate_annotations

annotations = [
    Exempt_Region("src/tipos.adb", "5:4", "12:39", "test exempting all"),
]

annot_file = generate_annotations(annotations)

# Check we get the expected results
TestCase(category=CAT.stmt).run(
    covcontrol=CovControl(
        covoptions=[f"--external-annotations={annot_file}"], auto_units=True
    )
)
thistest.result()
