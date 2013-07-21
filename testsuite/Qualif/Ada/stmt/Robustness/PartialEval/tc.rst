short-circuiting part of a statement's execution (expression evaluation)
========================================================================

Check that the tool isn't fooled into thinking that a statement is not covered
just because part of its execution (expression evaluation) is shortcircuited.

Verify that ``return A and then B`` is not reported uncovered when
exercised with A False only.



