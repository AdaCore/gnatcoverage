MCDC requirements for the combination of multiple execution traces
==================================================================

.. rubric:: Requirement(s)

When the coverage achieved by multiple execution traces is evaluated, an mcdc
violation shall only be reported when it would have been for all the traces
individually.


.. rubric:: Testing Strategy



We exercise consolidation of traces obtained for single vector invocations
over a range of basic decisions.

For decisions with two operands, first run with every possible input vector
independently, then check all the possible combinations of those input vectors
without repetitions.

We operate similarily for decisions with three operands, except we aggregate
inputs that differ only on non-evaluated conditions.


.. qmlink:: TCIndexImporter

   *


