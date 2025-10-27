from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest

TestCase(
    category=CAT.mcdc,
    fun_call_lvl=True,
    tolerate_messages=(
        r"cannot instrument calls within dotted names|cannot instrument an"
        r" expression function which is a primitive of its return type, when"
        r" this type is a tagged type. Consider turning it into a regular"
        r" function body."
    ),
).run()
thistest.result()
