from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

TestCase(
    category=CAT.mcdc,
    fun_call_lvl=True,
    tolerate_messages="cannot instrument calls to user-defined operators",
).run()
thistest.result()
