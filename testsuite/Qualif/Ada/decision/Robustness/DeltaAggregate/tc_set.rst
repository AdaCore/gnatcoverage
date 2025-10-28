SC + DC on various decisions nested in delta aggregates
=======================================================

Exercise decision coverage on decisions nested within both array and record
delta aggregates. Check for correct handling of nested decisions within the
base expression, in the index expression as well as in the component value
expression. The tests are organized such that each test exercises all three
possible decisions in a delta aggregate for the construct corresponding to the
test directory name.

The Test_Pkg_0 driver tests ensures only SC violations are reported when the
code in the unit of interest is not executed, Test_Pkg_Base verifies proper DC
of decisions present in the base expression of the delta aggregate,
Test_Pkg_Comp ensures proper DC computation for decisions nested in the
component index expression of the delta aggregate, Test_Pkg_Value ensures that
decisions within the component value expression are properly processed, and
Test_Pkg_Full demonstrates decision coverage being exercised for all three
possible nested decisions at the same time.
