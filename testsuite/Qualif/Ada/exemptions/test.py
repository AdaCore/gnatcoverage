import os
from test_utils import *

# Check that a simple decision SCO is correctly loaded and interpreted
# by xcov.

gprbuild (gprfor (['hello.adb']))

xcov (['map-routines', '--scos=obj/hello.ali'], "out")

thistest.fail_if (differs ("expected", "out"),
                  "exemption pragma placement rules")

thistest.result()
