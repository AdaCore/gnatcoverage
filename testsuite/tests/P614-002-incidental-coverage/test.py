from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


# Make SCOV.tc instrument all sources so that consolidation with traces do show
# incindental coverage for the Math unit (i.e. the trace produced by the Maps
# testcase contributes to the coverage of the Mapth unit).
thistest.options.gprmode = False

TestCase(category=CAT.decision).run()
thistest.result()
