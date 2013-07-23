sequences of multiple nop statements (e.g. null or pragma)
===========================================================

Check that sequences of multiple "no-op" statements (constructs with
no generated code, such as null statements or pragma) are
handled properly.

Exercise a package containing a subprogram with two consecutive "no-op"
statements followed by a statement with code.

Verify that the two statements with no code are reported with the same coverage
status as the subsequent statement with code.

