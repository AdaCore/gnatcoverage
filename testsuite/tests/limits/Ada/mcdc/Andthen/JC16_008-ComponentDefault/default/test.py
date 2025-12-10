import re

from SCOV.tc import TestCase
from SUITE.context import thistest


TestCase(
    tolerate_messages=re.escape(
        "!!! andthen.adb:4:26: gnatcov limitation: cannot find local"
        " declarative part for MC/DC"
    )
).run()
thistest.result()
