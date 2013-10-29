MCDC requirements for mixed statement and decision constructs
=============================================================

.. rubric:: Requirement(s)

The Core requirements are honored on programs mixing arbitrary Ada constructs
together, with arbitrary levels of syntactic nesting (such as loops within
tests within subprograms etc).


.. rubric:: Testing Strategy

We exercise multiple cases of functional code featuring a variety of
constructs nested within each other (FOR, WHILE, CASE, IF), and for every case
check that the behavior matches expectations in a wide range of possible
situations:

* Loops entered or not,
* Possible case selections taken alone or combined with others,
* IF controls evaluated True only, False only or both,
* Combinations of these allowed by the nesting structure.


.. qmlink:: TCIndexImporter

   *


