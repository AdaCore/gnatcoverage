"""
Test the behaviour of the --source-rebase option under various configurations
for Ada projects.
"""

from SUITE.context import thistest
from test_support import run_variant

# Test that specifiying a wrong rebase prefix still outputs warnings about
# files not found.
run_variant(
    variant_basename="no_src",
    mains_list=["disjoint_main_1.adb"],
    units_lists=[["Pkg1"]],
    origin_src_dir="src",
    expected_xcov_results={},
    rebase_dir="/some/random/dir",
    expect_failure=True,
    suppress_src_dir=True,
)

# Test that specifying a wrong rebase prefix outputs no warning if the original
# sources are still on the disk.
run_variant(
    variant_basename="no_coverage_src",
    mains_list=["disjoint_main_1.adb"],
    units_lists=[["Pkg1"]],
    origin_src_dir="src",
    expected_xcov_results={
        "pkg1.ads.xcov": {},
        "pkg1.adb.xcov": {'+': {8, 9, 11}}
    },
    rebase_dir="/some/random/dir",
)

# Check the absence of warnings when the source rebase does redirect to
# existing sources.
run_variant(
    variant_basename="simple",
    mains_list=["disjoint_main_1.adb"],
    units_lists=[["Pkg1"]],
    origin_src_dir="src",
    expected_xcov_results={
        "pkg1.ads.xcov": {},
        "pkg1.adb.xcov": {'+': {8, 9, 11}}
    },
    suppress_src_dir=True,
)

# Check that source rebase is done correctly when rebasing sources from two
# checkpoints, concerning disjoint set of sources for each checkpoint.
run_variant(
    variant_basename="disjoint",
    mains_list=["disjoint_main_1.adb", "disjoint_main_2.adb"],
    units_lists=[["Pkg1"], ["Pkg2"]],
    origin_src_dir="src",
    expected_xcov_results={
        "pkg1.ads.xcov": {},
        "pkg1.adb.xcov": {'+': {8, 9, 11}},
        "pkg2.ads.xcov": {},
        "pkg2.adb.xcov": {'+': {8, 9, 11}}
    },
    suppress_src_dir=True,
)

# Check that source rebase is done correctly when rebasing sources from two
# checkpoints, where the set of sources from each checkpoint is overlapping.
# In particular, check that gnatcov doesn't consider that a same file coming
# from two different checkpoints is two separate files.
#
# This is also useful to check that source-rebase actually rebases when both
# the original and the rebase source files exist. Indeed, if GNATcoverage did
# not rebase, we would get ``Warning: same base name for files`` as both
# checkpoints would hold the same units, with the same base name but with a
# different full name. As by default, warnings are registered as errors in a
# test execution, this would fail.
run_variant(
    variant_basename="intersecting",
    mains_list=["intersecting_main_1.adb", "intersecting_main_2.adb"],
    units_lists=[["Pkg1", "Pkg2"]] * 2,
    origin_src_dir="src",
    expected_xcov_results={
        "pkg1.ads.xcov": {},
        "pkg1.adb.xcov": {'+': {8, 9, 11}},
        "pkg2.ads.xcov": {},
        "pkg2.adb.xcov": {'+': {8, 9, 11}}
    },
)

thistest.result()
