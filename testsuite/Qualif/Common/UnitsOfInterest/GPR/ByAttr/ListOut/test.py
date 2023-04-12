from SCOV.tc import TestCase
from SCOV.tctl import CovControl
from SUITE.context import thistest
from SUITE.cutils import Wdir, list_to_file

base_out = ["test_or_ft", "test_and_tt", "test_and_tf"]

wd = Wdir()

# Check on lone node unit only
wd.to_subdir("wd_1")
TestCase(category=None).run(
    covcontrol=CovControl(ulist_out="../" + list_to_file(base_out + ["ops"]),
                          xreports=["ops-andthen.adb", "ops-orelse.adb"]))

# Check on child units only
wd.to_subdir("wd_2")
TestCase(category=None).run(covcontrol=CovControl(
    ulist_out="../" + list_to_file(base_out + ["ops.orelse", "ops.andthen"]),
    xreports=["ops.ads", "ops.adb"]))

# Check on root + child unit
wd.to_subdir("wd_3")
TestCase(category=None).run(covcontrol=CovControl(
    ulist_out="../" + list_to_file(base_out + ["ops", "ops.andthen"]),
    xreports=["ops-orelse.adb"]))

thistest.result()
