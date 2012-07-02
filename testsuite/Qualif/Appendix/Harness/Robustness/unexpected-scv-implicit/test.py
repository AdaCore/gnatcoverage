from SCOV.tc import *
TestCase(category="stmt").run()

from SCOV.harness import *

HarnessMonitor (
    expected_diags = [
        HarnessDiagnostic (
            text = 'Unexpected sNoCov mark at sloc 3:4'),
        HarnessDiagnostic (
            text = '(inc.adb.xcov) Unexpected lNoCov mark at line 3'),
        ]
    ).run ()

thistest.result()
