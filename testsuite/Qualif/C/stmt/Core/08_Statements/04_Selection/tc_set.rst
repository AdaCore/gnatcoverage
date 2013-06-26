SC expectations regarding section 8.4: Selection statements
===========================================================

SC expectations regarding section 8.4: Selection statements

This sub-section of the standard describes several “conditionnal” structures
that execute different statements depending on the value of some expression.

%(subset-index)s

%(tstrategy-headline)s

Each kind of selection instruction has its pecularities, but they are exercised
following a common strategy:

-   check that nothing is covered if nothing is executed
-   when reasonnable, exercise the instruction for each possible value of the
    controlling expression, and check that the correct set of nested statements
    is executed
-   do the same with multiple branches executed in the same test driver

%(tc-index)s
