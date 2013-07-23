SC expectations for mixed statement constructs
===============================================

SC expectations for code comprising different kinds of statements,
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

* IF statements that only evaluate to True, that only evaluate to False
  or that evaluate to both values,

* Combinations of these allowed by the nesting structure.


.. qmlink:: TCIndexImporter

   *



