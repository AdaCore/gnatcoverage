from SCOV.htc import *
from SCOV.tctl import CAT

# The =report diags we always expect. The =xcov diags we only expect in
# !qualif runs as these don't produce the =xcov outputs.

expected_diags = [
    HarnessDiagnostic (
        text = "Missing expected sNoCov mark at line 3")
    ]

if not thistest.options.qualif_level:
    expected_diags.append (
        HarnessDiagnostic (
            text = "Missing expected lNoCov mark at line 3")
        )

HarnessTestCase(
    expected_diags=expected_diags,
    category=CAT.stmt).run()

thistest.result()
