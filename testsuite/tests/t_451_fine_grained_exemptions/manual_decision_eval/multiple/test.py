"""
Check passing multiple manual decision evaluations.
"""

import os.path

from SCOV.tc import TestCase
from SCOV.tctl import CovControl, CAT
from SUITE.context import thistest


TestCase(category=CAT.mcdc).run(
    covcontrol=CovControl(
        covoptions=[
            "--external-annotations",
            os.path.abspath("annotations.toml"),
        ],
        auto_units=True,
    )
)
thistest.result()
