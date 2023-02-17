from SCOV.tc import TestCase
from SCOV.tctl import CAT, CovControl
from SUITE.context import thistest


TestCase(category=CAT.decision).run(covcontrol=CovControl
                                    (covoptions="--all-messages"))
thistest.result()
