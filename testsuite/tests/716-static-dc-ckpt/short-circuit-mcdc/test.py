from pathlib import Path
import sys

from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.tutils import xcov

# Add the path of test utils to import it
sys.path.append(
    str(Path(__file__).absolute().parent.parent.joinpath("common").resolve())
)

from test_utils import (
    DoubleConditionCheckpointBuilder,
    TestCaseRunner,
)
from test_utils import PkgType as PT

wd = Wdir("tmp_")

COV_LEVEL = "stmt+mcdc"

ckb = DoubleConditionCheckpointBuilder(COV_LEVEL)
tc_runner = TestCaseRunner(COV_LEVEL)

# Fully static
st_st = ckb.new(PT.ST, PT.ST)  # decision is true
sf_sf = ckb.new(PT.SF, PT.SF)  # decision is false

#############
# Test case #
#############
# Ensure that when consolidating static `true || true` and `false || false`
# will raise a violation on the second condition but not on the first one
report_file = "report.txt"
args = [
    "coverage",
    "--annotate=report",
    "-o",
    f"{report_file}",
    f"--level={COV_LEVEL}",
    "-C",
    f"{st_st}",
    "-C",
    f"{sf_sf}",
]
xcov(args, "output.log")

with open(report_file, "r") as file:
    content = file.read()

# Fail if the first condition is not covered
thistest.fail_if(content.find("greet.adb:11:16: condition") != -1)
# Fail if the second condition is covered
thistest.fail_if(content.find("greet.adb:11:33: condition") == -1)

thistest.result()
