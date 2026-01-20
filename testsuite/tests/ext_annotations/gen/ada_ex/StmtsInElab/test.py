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
    Exempt_Region("src/com.adb", "19:7", "22:42", "auto init off"),
    Exempt_On("src/gcom.adb", "16:7", None, "auto init off"),
    Exempt_Off(
        "src/gcom.adb",
        "19:42",
        None,
    ),
]

annot_file = generate_annotations(annotations)

# Check we get the expected results
TestCase(category=CAT.stmt).run(
    covcontrol=CovControl(
        covoptions=[f"--external-annotations={annot_file}"], auto_units=True
    )
)
thistest.result()
