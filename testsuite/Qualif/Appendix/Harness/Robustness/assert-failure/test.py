from SCOV.tc import *
TestCase(category="stmt").run()

from SCOV.harness import *

HarnessMonitor (
    expected_diags = [
        HarnessDiagnostic (
            text = "exception raised while running 'test_afail'"
            )
        ]
    ).run ()

thistest.result()
