Check the correctness of DC assessments on decisions that are not entirely evaluated (don't yield
=================================================================================================

Check the correctness of DC assessments on decisions that are not entirely evaluated (don't yield
an outcome) even though the statement they control is executed, typically when
evaluation is interrupted by an exception occurrence.

We exercise a range of expressions where each condition could be made to raise
an exception in a panel of cases, checking that

* a ``decision never evaluated`` diagnostic is emitted when no evaluation
  succeeds at all, and that

* exception occurrences don't influence the coverage achieved by otherwise
  successful evaluations.

%(tc-index)s

