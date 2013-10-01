from SCOV.htc import *
from SCOV.tctl import CAT

HarnessTestCase(
    expected_diags = [
        HarnessDiagnostic (
            text = "exception raised while running 'test_afail'"
            )
        ],
    category=CAT.stmt).run()

thistest.result()
