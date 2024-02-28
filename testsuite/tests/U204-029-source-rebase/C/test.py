"""
Test the behaviour of the --source-rebase option under various configurations
for C projects.
"""

from SUITE.context import thistest
from test_support import run_variant


# Check that source rebase is done correctly when rebasing sources from two
# checkpoints, where the set of sources from each checkpoint is overlapping.
# In particular, check that gnatcov doesn't consider that a same file comming
# from two different checkpoints is two separate files.
run_variant(
    variant_basename="intersecting",
    mains_list=["main1.c", "main2.c"],
    units_lists=[
        [
            "main1.c",
        ],
        ["main2.c"],
    ],
    origin_src_dir="src",
    expected_xcov_results={
        "main1.c.xcov": {"+": {7, 8, 9}},
        "main2.c.xcov": {"+": {7, 8, 9}},
        "a-pkg.h.xcov": {"+": {5, 7, 11}},
        "b-pkg.h.xcov": {"+": {5, 7, 11}},
    },
)


thistest.result()
