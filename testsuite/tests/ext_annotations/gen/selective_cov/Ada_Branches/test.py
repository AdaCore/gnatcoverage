"""
Check gnatcov support for selective instrumentation of Ada branching
statements, using external annotations.

Regression test: the branch info computed for the alternatives of IF and CASE
statements used to only account for in-source Cov_Off annotations, and gnatcov
crashed when instrumenting an IF statement lying in a region disabled through
an external annotation.
"""

from SUITE.context import thistest
from SCOV.tctl import CAT, CovControl
from SCOV.tc import TestCase

from SUITE.tutils import Cov_Off, Cov_On, generate_annotations

src = "src/test_main.adb"

# The locations below refer to src/test_main.adb: Cov_Off designates the first
# token of the disabled statement, and Cov_On the location right past the
# "end if;"/"end case;" that closes it.
annotations = generate_annotations(
    [
        # Whole IF statement in All_Disabled
        Cov_Off(src, "13:7", None, "whole if statement"),
        Cov_On(src, "19:14", None),
        # Only the ELSE part in Else_Disabled
        Cov_Off(src, "29:7", None, "else part only"),
        Cov_On(src, "31:14", None),
        # Whole CASE statement in Case_Disabled
        Cov_Off(src, "38:7", None, "whole case statement"),
        Cov_On(src, "41:16", None),
    ]
)

TestCase(category=CAT.mcdc).run(
    CovControl(instroptions=f"--external-annotations={annotations}")
)

thistest.result()
