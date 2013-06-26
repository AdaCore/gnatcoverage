SC expectations regarding Return statements
===========================================

SC expectations regarding Return statements

%(req-headline)s

A RETURN statement is both a statement per se and a flow-control
operator. 

* A RETURN statement shall be reported as uncovered when it is not executed,

* When a RETURN statement is executed, all the statements in the subprogram
  body that have not been executed by that moment shall be reported uncovered.

%(tstrategy-headline)s

We exercise subprograms that feature one or several return statements in
various contexts, conditional or not, arranging to get into combinations
of the possible execution flow variants.

%(tc-index)s
