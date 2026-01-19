from SCOV.tc import TestCase
from SCOV.tctl import CovControl, CAT
from SUITE.context import thistest

from SUITE.tutils import (
    Exempt_On,
    Exempt_Off,
    Exempt_Region,
    generate_annotations,
)

# Annotations to be used, use a mix of Exempt_Region an Exempt_On/Off
annotations = [
    Exempt_Region(
        "src/multiple_exemptions.adb", "26:7", "29:42", "exemption section #1"
    ),
    Exempt_Region(
        "src/multiple_exemptions.adb", "36:10", "39:45", "exemption section #2"
    ),
    Exempt_On(
        "src/multiple_exemptions.adb", "48:10", None, "exemption section #3"
    ),
    Exempt_Off("src/multiple_exemptions.adb", "51:45", None),
    Exempt_On(
        "src/multiple_exemptions.adb", "62:7", None, "exemption section #4"
    ),
    Exempt_Off("src/multiple_exemptions.adb", "68:42", None),
]

annot_file = generate_annotations(annotations)

# Check we get the expected results
TestCase(category=CAT.stmt).run(
    covcontrol=CovControl(
        covoptions=[f"--external-annotations={annot_file}"], auto_units=True
    )
)
thistest.result()
