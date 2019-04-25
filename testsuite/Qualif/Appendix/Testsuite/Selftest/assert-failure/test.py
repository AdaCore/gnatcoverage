from SCOV.htc import HarnessDiagnostic, HarnessTestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


HarnessTestCase(
    expected_diags=[HarnessDiagnostic(
        text="exception raised while running 'test_afail'")],
    category=CAT.stmt).run()
thistest.result()
