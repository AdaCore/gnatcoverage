"""
This is a complement to the existing tests for generics in the Qualif part of
the testsuite. These are grouped in the stmt coverage part and focus on the
distinction between the default mode and -S instance, assuming that the
combination circuits are orthogonal to the criteria.

Here we mean to test -S routine as well, and take the opportunity to test with
a different criterion while we're at it.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT, CovControl
from SUITE.context import thistest


tc = TestCase(category=CAT.mcdc)

tc.run()
tc.run(covcontrol=CovControl(covoptions="-S instance"), subdirhint="r_")

# -S routine is intractable with inlining
if "-gnatn" not in thistest.suite_cargs_for("Ada"):
    tc.run(covcontrol=CovControl(covoptions="-S routine"), subdirhint="r_")

thistest.result()
