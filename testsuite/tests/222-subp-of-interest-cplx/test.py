"""
Test that the scope mechanism (and thus the subprogram of interest feature)
correctly traverses the scopes, even when the traversal is not done with SCOs
in a contiguous order.

The One subprogram in the sources used to trigger two related issues:
 - The decision in One spanning multiple lines, the scope traversal was done
   back and forth on the statement SCO then the decision SCO on each line, thus
   breaking the precondition for Traverse_SCO;

 - The scope traversal when loading the trace was done on the first covered
   statement SCO, which is not in the first scope. The trace entries were thus
   silently discarded.

   More details bellow, making references to the old implementation.

   To be more precise, the previous implementation relied on forward iterators
   in multi-way trees to traverse the scope tree, in depth first order until it
   found a scope that did not contain the traversed SCO. When loading the
   coverage buffers from a given trace, Traverse_SCO is only called on SCOs for
   which the correspond buffer bit is set. As such, in this example, the first
   SCO on which Traverse_SCO will be called is the statement SCO for the return
   statement in One.

   When initiating the traversal, Next_SE point to the scope for the whole
   CU, which contains the SCO of interest. The traversal then moves to the next
   one, which is the scope for Zero. This does not contain SCO, so the
   traversal stops, and current_SE thus points to the scope for the whole CU.
   While it is true that this scope contains SCO, it is not the deepest one.
   The current scope in the traversal is then checked against the set of scopes
   of interest, which only consist in the scope for One, so this returns False.
   The coverage information is then wrongfully discarded.

This test checks both bugs.
"""

from SCOV.minicheck import build_run_and_coverage, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches

tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor(mains=["main.adb"], srcdirs=[".."]), units=["Pkg"]
    ),
    covlevel="stmt+decision",
    mains=["main"],
    extra_coverage_args=["--subprograms=../pkg.ads:3", "-axcov"],
)

check_xcov_reports(
    "obj", {"pkg.ads.xcov": {}, "pkg.adb.xcov": {"!": {12, 13}, "+": {14, 15}}}
)

thistest.result()
