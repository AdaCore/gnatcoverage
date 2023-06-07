from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


for cat in CAT.critcats:
    TestCase(
        # Note that when C support for GNATcoverage will be ready, this flag
        # should be automatically enabled with -fdump-scos.
        extracargs='-no-integrated-cpp',
        category=cat
    ).run()
thistest.result()
