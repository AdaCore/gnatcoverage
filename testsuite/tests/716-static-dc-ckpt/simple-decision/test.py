from pathlib import Path
import sys

from SUITE.context import thistest
from SUITE.cutils import Wdir

# Add the path of test utils to import it
sys.path.append(
    str(Path(__file__).absolute().parent.parent.joinpath("common").resolve())
)

from test_utils import (
    SimpleConditionCheckpointBuilder,
    TestCaseRunner,
)
from test_utils import PkgType as PT

wd = Wdir("tmp_")


COV_LEVEL = "stmt+decision"

ckb = SimpleConditionCheckpointBuilder(COV_LEVEL)
tc_runner = TestCaseRunner(COV_LEVEL)

# Create two checkpoints with different coverage levels
st = ckb.new(PT.ST)
sf = ckb.new(PT.SF)
dt = ckb.new(PT.DT)

#################
# TEST CASE 1.1 #
#################
# Ensure that making a report from a static checkpoint only does not
# report any violation.
tc_runner.run_test_case("Test Case 1.1", [sf], ckb.EXPECT_EMPTY)

#################
# TEST CASE 1.2 #
#################
# Ensure that consolidating twice the same static decision does not
# report any violation.
tc_runner.run_test_case("Test Case 1.2", [sf, sf], ckb.EXPECT_EMPTY)

###############
# TEST CASE 2 #
###############
# Ensure that making a report from a dynamic checkpoint reports a
# decision violation.
tc_runner.run_test_case("Test Case 2", [dt], ckb.EXPECT_VIOLATION)

###############
# TEST CASE 3 #
###############
# Ensure that making a consolidated report from both static checkpoints
# does not raise any violation
tc_runner.run_test_case("Test Case 3", [sf, st], ckb.EXPECT_EMPTY)

#################
# TEST CASE 4.1 #
#################
# Ensure that making a consolidated report from the true static checkpoint
# and the dynamic one (which evaluates to True) raises a decision violation
tc_runner.run_test_case("Test Case 4.1", [st, dt], ckb.EXPECT_VIOLATION)

#################
# TEST CASE 4.2 #
#################
# Ensure the commutativity of checkpoint-loading order
tc_runner.run_test_case("Test Case 4.2", [dt, st], ckb.EXPECT_VIOLATION)

#################
# TEST CASE 5.1 #
#################
# Ensure that making a consolidated report from the false static checkpoint
# and the dynamic one (which evaluates to True) covers the decision
tc_runner.run_test_case("Test Case 5.1", [sf, dt], ckb.EXPECT_EMPTY)

#################
# TEST CASE 5.2 #
#################
# Ensure the commutativity of checkpoint-loading order
tc_runner.run_test_case("Test Case 5.2", [dt, sf], ckb.EXPECT_EMPTY)

thistest.result()
