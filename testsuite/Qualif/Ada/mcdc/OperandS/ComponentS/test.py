from test_utils import *

ExerciseAll(extradrivers=
            "../../src/test_andthen*.adb"
            + " ../../src/test_orelse*.adb"
            + " ../../src/test_pandpor*.adb")
thistest.result()
