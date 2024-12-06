from pathlib import Path
import sys

from SUITE.context import thistest
from SUITE.cutils import Wdir

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

COV_LEVEL = "stmt+decision"

ckb = DoubleConditionCheckpointBuilder(COV_LEVEL)
tc_runner = TestCaseRunner(COV_LEVEL)

# Checkpoint naming follows the convention:
# <first_bool>_<second-bool>

# Fully static
st_st = ckb.new(PT.ST, PT.ST)  # decision is true
st_sf = ckb.new(PT.ST, PT.SF)  # decision is true
sf_st = ckb.new(PT.SF, PT.ST)  # decision is true
sf_sf = ckb.new(PT.SF, PT.SF)  # decision is false
# Fully dynamic
dt_dt = ckb.new(PT.DT, PT.DT)  # decision is true
dt_df = ckb.new(PT.DT, PT.DF)  # decision is true
df_dt = ckb.new(PT.DF, PT.DT)  # decision is true
df_df = ckb.new(PT.DF, PT.DF)  # decision is false
# Partly static
dt_sf = ckb.new(PT.DT, PT.SF)  # decision is false

#############
# Test case #
#############
# Fully static decisions alone never raise violations
tc_runner.run_test_case("simple-static-1", [st_st], ckb.EXPECT_EMPTY)
tc_runner.run_test_case("simple-static-2", [st_sf], ckb.EXPECT_EMPTY)
tc_runner.run_test_case("simple-static-3", [sf_st], ckb.EXPECT_EMPTY)
tc_runner.run_test_case("simple-static-4", [sf_sf], ckb.EXPECT_EMPTY)

#############
# Test case #
#############
# Dynamic decisions alone will raise violations
tc_runner.run_test_case("simple-dynamic-1", [dt_dt], ckb.EXPECT_VIOLATION)
tc_runner.run_test_case("simple-dynamic-2", [dt_df], ckb.EXPECT_VIOLATION)
tc_runner.run_test_case("simple-dynamic-3", [df_dt], ckb.EXPECT_VIOLATION)
tc_runner.run_test_case("simple-dynamic-4", [df_df], ckb.EXPECT_VIOLATION)

#############
# Test case #
#############
# Partly static non-covering checkpoint raises a violation
tc_runner.run_test_case("simple_partly_static", [dt_sf], ckb.EXPECT_VIOLATION)

#############
# Test case #
#############
# Ensure consolidating twice the same non-covering static decision evaluation
# does not raise a violation
tc_runner.run_test_case(
    "OK_duplicate_static", [sf_sf, sf_sf], ckb.EXPECT_EMPTY
)

#############
# Test case #
#############
# Consolidating several different static checkpoints that don't cover the
# decision raise a violation
tc_runner.run_test_case("KO_static", [sf_st, st_sf], ckb.EXPECT_VIOLATION)

#############
# Test case #
#############
# Consolidating several different static checkpoints that don't cover the
# decision pass
tc_runner.run_test_case("OK_static", [sf_sf, sf_st], ckb.EXPECT_EMPTY)

#############
# Test case #
#############
# Consolidating static and dynamic checkpoints that cover the decision
# raises no violation
tc_runner.run_test_case(
    "OK_dynamic_static-1", [dt_df, sf_sf], ckb.EXPECT_EMPTY
)
tc_runner.run_test_case(
    "OK_dynamic_static-2", [sf_sf, dt_dt], ckb.EXPECT_EMPTY
)

#############
# Test case #
#############
# Consolidating static and dynamic checkpoints that don't cover the decision
# raises a violation
tc_runner.run_test_case(
    "KO_dynamic_static-1", [dt_df, st_sf], ckb.EXPECT_VIOLATION
)
tc_runner.run_test_case(
    "KO_dynamic_static-2", [st_sf, dt_df], ckb.EXPECT_VIOLATION
)

#############
# Test case #
#############
# Consolidating partly static and static checkpoints
# OK
tc_runner.run_test_case(
    "OK_partly_static_static-1", [dt_sf, sf_sf], ckb.EXPECT_EMPTY
)
tc_runner.run_test_case(
    "OK_partly_static_static-2", [sf_sf, dt_sf], ckb.EXPECT_EMPTY
)
# KO
tc_runner.run_test_case(
    "KO_partly_static_static-1", [dt_sf, st_sf], ckb.EXPECT_VIOLATION
)
tc_runner.run_test_case(
    "KO_partly_static_static-2", [st_sf, dt_sf], ckb.EXPECT_VIOLATION
)

#############
# Test case #
#############
# Consolidating partly static and dynamic checkpoints
# OK
tc_runner.run_test_case(
    "OK_partly_static_dynamic-1", [dt_sf, df_df], ckb.EXPECT_EMPTY
)
tc_runner.run_test_case(
    "OK_partly_static_dynamic-2", [df_df, dt_sf], ckb.EXPECT_EMPTY
)
# KO
tc_runner.run_test_case(
    "KO_partly_static_dynamic-1", [dt_sf, dt_df], ckb.EXPECT_VIOLATION
)
tc_runner.run_test_case(
    "KO_partly_static_dynamic-2", [dt_df, dt_sf], ckb.EXPECT_VIOLATION
)

thistest.result()
