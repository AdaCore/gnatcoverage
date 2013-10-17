**short-circuiting part of a statement's execution (expression evaluation)**

Check for correct coverage analysis in the presence of short-circuit
control forms. In particular, check that short-circuiting an expression
evaluation does not result in the enclosing statement being treated
as uncovered.

Verify that "return A and then B" is not reported uncovered when
exercised with A False only.



