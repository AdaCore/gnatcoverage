from SCOV.tc import *
from SCOV.tctl import CAT

TestCase(category=CAT.stmt).run()

from SCOV.harness import *

HarnessMonitor (
    expected_diags = [
        HarnessDiagnostic (
            text = "Missing expected sNoCov mark at line 3",
            ),
        HarnessDiagnostic (
            text = "(inc.adb.xcov) Missing expected lNoCov mark at line 3",
            )
        ]
    ).run ()

thistest.result()
