"""
Check that the instrumentation of expression functions does not trigger a GNAT
miscompilation bug. The instrumented test program used to crash with a
Program_Error/Storage_Error.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.stmt).run()
thistest.result()
