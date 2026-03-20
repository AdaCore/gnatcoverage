from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

TestCase(
    # Note that when C support for GNATcoverage will be ready, this flag
    # should be automatically enabled with -fdump-scos.
    extracargs="-no-integrated-cpp",
    category=CAT.stmt,
).run()
thistest.result()
