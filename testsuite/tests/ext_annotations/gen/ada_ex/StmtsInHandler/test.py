from SCOV.tc import TestCase
from SCOV.tctl import CovControl, CAT
from SUITE.context import thistest

from SUITE.tutils import Exempt_Region, generate_annotations

# Annotations to be used, use a mix of Exempt_Region an Exempt_On/Off
annotations = [
    Exempt_Region("src/stacks.adb", "27:10", "30:45", "exception handler"),
]

annot_file = generate_annotations(annotations)

# Check we get the expected results
TestCase(category=CAT.stmt).run(
    covcontrol=CovControl(
        covoptions=[f"--external-annotations={annot_file}"], auto_units=True
    )
)
thistest.result()
