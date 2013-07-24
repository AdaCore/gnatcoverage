DC expectations for mixed statement and decision constructs
============================================================

DC expectations for combinations of statement and decision constructs
representative of application code.


Requirement(s)
--------------


Correct coverage is reported for programs that use a combination of
Ada constructs, with arbitrary levels of syntactic nesting
(such as loops within tests within subprograms).


Testing Strategy
----------------



We exercise multiple cases with a variety of
constructs nested within each other (For, While, Case, If), and for each case
verify the behavior in a wide range of contexts:

* Loops, entered or not,

* Possible case selections taken alone or combined with others,

* IF statements that only evaluate to true, that only evaluate to False
  or that evaluate to both values,

* Combinations of these allowed by the nesting structure.


.. qmlink:: TCIndexImporter

   *



