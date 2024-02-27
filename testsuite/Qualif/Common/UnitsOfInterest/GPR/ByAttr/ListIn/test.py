from SCOV.tc import TestCase
from SCOV.tctl import CovControl
from SUITE.context import thistest
from SUITE.cutils import Wdir, list_to_file

wd = Wdir()

# Check on lone node unit only

wd.to_subdir("wd_1")

TestCase(category=None).run(
    covcontrol=CovControl(ulist_in="../" + list_to_file(["ops"]),
                          xreports=["ops.ads", "ops.adb"]))

# Check on lone node + child unit
wd.to_subdir("wd_2")
TestCase(category=None).run(covcontrol=CovControl(
    ulist_in="../" + list_to_file(["ops", "ops.andthen"]),
    xreports=["ops.ads", "ops.adb", "ops-andthen.ads", "ops-andthen.adb"]))

# Check on lone child unit only
wd.to_subdir("wd_3")
TestCase(category=None).run(
    covcontrol=CovControl(ulist_in="../" + list_to_file(["ops.andthen"]),
                          xreports=["ops-andthen.ads", "ops-andthen.adb"]))

thistest.result()
