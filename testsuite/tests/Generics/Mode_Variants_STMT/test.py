from SCOV.tc import TestCase
from SCOV.tctl import CAT, CovControl
from SUITE.context import thistest


tc = TestCase(category=CAT.stmt)

tc.run()

tc.run(covcontrol=CovControl(covoptions="-S instance"),
       subdirhint="i_")

# -S routine is intractable with inlining
if '-gnatn' not in thistest.suite_cargs_for('Ada'):
    tc.run(covcontrol=CovControl(covoptions="-S routine"),
           subdirhint="r_")

thistest.result()
