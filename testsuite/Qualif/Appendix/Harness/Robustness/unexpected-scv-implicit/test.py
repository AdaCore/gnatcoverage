from SCOV.htc import *
from SCOV.tctl import CAT

HarnessTestCase(
    expected_diags = [
        HarnessDiagnostic (
            text = 'Unexpected sNoCov mark at sloc 3:4'),
        HarnessDiagnostic (
            text = '(inc.adb.xcov) Unexpected lNoCov mark at line 3'),
        ],
    category=CAT.stmt).run()

thistest.result()
