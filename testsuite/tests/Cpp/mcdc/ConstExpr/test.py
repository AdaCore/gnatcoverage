from SCOV.tc import TestCase
from SUITE.context import thistest

TestCase(tolerate_messages=r".* cannot instrument constexpr").run()
thistest.result()
