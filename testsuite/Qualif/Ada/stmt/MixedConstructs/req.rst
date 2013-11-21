SC requirements for mixed statement constructs
==============================================

.. rubric:: Requirement

Correct coverage shall be reported for programs that use combinations of Ada
constructs with arbitrary levels of syntactic nesting, for example on
statements nested within LOOPs within IF constructs within subprograms.


.. rubric:: Testing Strategy

We exercise multiple cases with a variety of
constructs nested within each other (FOR, WHILE, CASE, IF), and for each case
verify the behavior in a wide range of contexts:

* Loops, entered or not,

* Possible case selections taken alone or combined with others,

* IF statements that only evaluate to True, that only evaluate to False
  or that evaluate to both values,

* Combinations of these allowed by the nesting structure.


.. qmlink:: TCIndexImporter

   *



