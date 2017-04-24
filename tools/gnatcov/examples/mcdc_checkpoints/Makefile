# This example illustrates the achivement of mcdc coverage thanks to
# checkpoints consolidation.

# The code features an and-then expression used in a return statement,
# considered a decision for the mcdc critierion. 3 individual tests exercise
# variants of True/False on the conditions, each leading to incomplete
# decision coverage while consolidation yields mcdc coverage.

TESTS=test_stable test_unstable_p test_unstable_t
XCOVLEVEL=stmt+mcdc
CONSOLIDATE=checkpoints

include ../support/Makefile.examples
