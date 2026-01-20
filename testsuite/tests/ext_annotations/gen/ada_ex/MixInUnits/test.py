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
        "src/exemptions.adb", "7:7", "10:42", "exemption on declarations"
    ),
    Exempt_Region(
        "src/exemptions.adb",
        "24:10",
        "28:45",
        "exemption on statements in function",
    ),
    Exempt_On(
        "src/exemptions.adb",
        "66:4",
        None,
        "exemption on elaboration code - 1",
    ),
    Exempt_Off("src/exemptions.adb", "70:39", None),
    Exempt_Region(
        "src/exemptions.adb",
        "74:4",
        "78:39",
        "exemption on elaboration code - 2",
    ),
    Exempt_Region(
        "src/exemptions.ads",
        "20:4",
        "23:39",
        "exemption on global declaration",
    ),
]

annot_file = generate_annotations(annotations)

# Check we get the expected results
TestCase(category=CAT.stmt).run(
    covcontrol=CovControl(
        covoptions=[f"--external-annotations={annot_file}"],
        auto_units=True,
    )
)
thistest.result()
