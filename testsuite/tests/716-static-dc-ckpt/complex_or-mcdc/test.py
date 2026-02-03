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

COV_LEVEL = "stmt+mcdc"

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
# Ensure consolidating twice the same non-covering static decision evaluation
# does not raise a violation
tc_runner.run_test_case(
    "OK_duplicate_static",
    [sf_sf, sf_sf],
    ckb.EXPECT_EMPTY,
)

#############
# Test case #
#############
# Covers the decision with static checkpoints only
tc_runner.run_test_case(
    "OK_static",
    [sf_sf, sf_st, st_sf],
    ckb.EXPECT_EMPTY,
)

#############
# Test case #
#############
# MCDC will raise a violation if several different static evaluations are
# merged and don't cover the decision
tc_runner.run_test_case(
    "KO_static",
    [sf_sf, sf_st],
    ckb.EXPECT_VIOLATION,
)

#############
# Test case #
#############
tc_runner.run_test_case(
    "OK_dynamic",
    [df_df, dt_df, df_dt],
    ckb.EXPECT_EMPTY,
)

#############
# Test case #
#############
# Ensure 'df_df' and 'dt_df' are not enough to cover the decision with MCDC
tc_runner.run_test_case(
    "KO_dynamic",
    [df_df, dt_df],
    ckb.EXPECT_VIOLATION,
)

#############
# Test case #
#############
tc_runner.run_test_case(
    "OK_2_dynamic_then_static",
    [df_df, dt_df, sf_st],
    ckb.EXPECT_EMPTY,
)

#############
# Test case #
#############
tc_runner.run_test_case(
    "KO_2_dynamic_then_static",
    [df_df, dt_df, st_sf],
    ckb.EXPECT_VIOLATION,
)

#############
# Test case #
#############
tc_runner.run_test_case(
    "OK_2_dynamic_then_partly_static",
    [df_df, df_dt, dt_sf],
    ckb.EXPECT_EMPTY,
)

#############
# Test case #
#############
tc_runner.run_test_case(
    "KO_2_dynamic_then_partly_static",
    [df_df, dt_df, dt_sf],
    ckb.EXPECT_VIOLATION,
)

#############
# Test case #
#############
tc_runner.run_test_case(
    "OK_2_static_then_dynamic",
    [sf_sf, st_sf, df_dt],
    ckb.EXPECT_EMPTY,
)

#############
# Test case #
#############
tc_runner.run_test_case(
    "KO_2_static_then_dynamic",
    [sf_sf, st_sf, dt_df],
    ckb.EXPECT_VIOLATION,
)

thistest.result()
