sequences of multiple nop statements (e.g. null or pragma)
===========================================================

Check that sequences of multiple nop statements (e.g. null or pragma) are
handled properly.

Exercise a package that exposes a subprogram containing two statements with no
associated code in sequence, followed by a statement with code.

Verify that the two statements with no code are reported with the same coverage
status as the following statement with code.

