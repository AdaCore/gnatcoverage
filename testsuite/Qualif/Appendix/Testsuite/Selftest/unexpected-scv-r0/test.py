from SCOV.htc import HarnessDiagnostic, HarnessTestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


HarnessTestCase(
    expected_diags=[
        HarnessDiagnostic(text='Unexpected sNoCov mark at sloc 3:4')],
    category=CAT.stmt).run()
thistest.result()
