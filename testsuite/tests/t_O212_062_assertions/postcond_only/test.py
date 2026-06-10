from SCOV.tc import TestCase
from SCOV.tctl import CAT, CovControl
from SUITE.context import thistest

covcontrol = CovControl(covoptions="--postcond-only", instroptions="")

# ATC
TestCase(category=CAT.stmt, assert_lvl="atc").run(covcontrol)
TestCase(category=CAT.decision, assert_lvl="atc").run(covcontrol)

# ATCC
TestCase(category=CAT.stmt, assert_lvl="atcc").run(covcontrol)
TestCase(category=CAT.decision, assert_lvl="atcc").run(covcontrol)
TestCase(category=CAT.mcdc, assert_lvl="atcc").run(covcontrol)

thistest.result()
