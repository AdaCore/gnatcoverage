SC expectations regarding section 8.6: Jump statements
======================================================

SC expectations regarding section 8.6: Jump statements

This sub-section of the standard describes several “jump” statements that
affects the execution flow when evaluated.

%(subset-index)s

%(tstrategy-headline)s

Each kind of “jump” statement has pecularities, but they are exercised
following a common strategy:

-   Statements that must appear inside a loop (CONTINUE, BREAK) are present
    twice in the same test loop.
-   Other ones (GOTO, RETURN) are also present twice in the same function.

Each test must check that only what is executed is tagged as covered. For this,
a test execute nothing, another one execute only one of the two statements, and
another one execute both statements, or the other one.
