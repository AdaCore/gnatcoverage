from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(
    category=CAT.mcdc,
    fun_call_lvl=True,
    tolerate_messages=(
        r"cannot instrument generic expression functions|"
        r"cannot instrument generic null procedures"
    ),
).run()
thistest.result()
