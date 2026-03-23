General requirements for SPARK Ghost code
=========================================

%REQ_ID%

gnatcov shall instrument ghost code iff the user passes the
`--instrument-ghost` switch. Otherwise, the tool shall not report coverage
violations on ghost entities.

The presence of ghost code in the analysis closure shall not
influence the analysis of non-ghost parts in that closure.

.. rubric:: Testing Strategy


Check the presence of the expected violations when the user passes the
`--instrument-ghost` switch and the absence thereof otherwise, for a variety of
ghost entities, such as
      
* An entire function,
* A whole package,
* Single statements in a regular (non-ghost) subprogram,
* A ghost Boolean initialized with a complex Boolean expression.

Verify that analysis on non ghost code still performs as expected, on
units containing no ghost code at all or a mix of ghost and non-ghost
code.
