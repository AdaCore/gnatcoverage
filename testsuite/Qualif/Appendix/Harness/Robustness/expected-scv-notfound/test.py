from SCOV.tc import *
from SCOV.tctl import CAT

TestCase(category=CAT.stmt).run()

from SCOV.harness import *

HarnessMonitor (
    expected_diags = [
        HarnessDiagnostic (
            text = "Expected sNoCov mark missing at line 3",
            ),
        HarnessDiagnostic (
            text = "(inc.adb.xcov) Expected lNoCov mark missing at line 3",
            )
        ]
    ).run ()

thistest.result()
