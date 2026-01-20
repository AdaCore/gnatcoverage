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
    Exempt_Region("src/stacks.adb", "7:13", "12:48", "no overflow"),
    Exempt_On("src/stacks.adb", "18:13", None, "no underflow"),
    Exempt_Off(
        "src/stacks.adb",
        "23:38",
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
