from SCOV.tc import *
from SCOV.tctl import CAT

TestCase(category=CAT.stmt).run()

from SCOV.harness import *

HarnessMonitor (
    expected_diags = [
        HarnessDiagnostic (
            text = "exception raised while running 'test_afail'"
            )
        ]
    ).run ()

thistest.result()
