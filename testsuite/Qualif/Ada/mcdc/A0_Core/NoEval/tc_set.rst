MCDC on decisions that are not entirely evaluated
=================================================

Check the correctness of MCDC assessments on decisions that are not entirely evaluated (don't
yield an outcome) even though the statement they control is executed,
typically when evaluation of a sub-decision is short-circuited.

We verify that "decision never evaluated" diagnostics are emitted
instead of statement coverage violations.


.. qmlink:: TCIndexImporter

   *


