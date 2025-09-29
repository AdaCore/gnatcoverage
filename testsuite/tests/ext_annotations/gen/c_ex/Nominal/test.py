"""
Test simple exemption cases in C sources. This is a copy of
tests/C/mcdc/Exemptions/Nominal, with the comments modified
to not be recognized, and external annotations being generated
instead.
"""

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
        "src/pkg.c", "6:4", "12:22", "whole function single line comment"
    ),
    Exempt_On("src/pkg.c", "17:4", None, "if stmt"),
    Exempt_Off("src/pkg.c", "20:7", None),
    Exempt_Region(
        "src/pkg.c", "32:3", "34:24", "whole function single line comment"
    ),
]

annot_file = generate_annotations(
    annotations,
    tolerate_messages=(
        r"warning: Could not create an auto-relocating annotation for src."
        "pkg.c:6:4 - 12:22, creating an absolute location annotation instead."
    ),
)


# Check we get the expected results
TestCase(category=CAT.mcdc).run(
    covcontrol=CovControl(covoptions=[f"--external-annotations={annot_file}"])
)
thistest.result()
