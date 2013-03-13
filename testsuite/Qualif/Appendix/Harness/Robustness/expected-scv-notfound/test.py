from SCOV.htc import *
from SCOV.tctl import CAT

HarnessTestCase(
    expected_diags = [
        HarnessDiagnostic (
            text = "Missing expected sNoCov mark at line 3",
            ),
        HarnessDiagnostic (
            text = "(inc.adb.xcov) Missing expected lNoCov mark at line 3",
            )
        ],
    category=CAT.stmt).run()

thistest.result()
