SC requirements for generalized Loop iterations (ARM 5.5.2)
===========================================================


%REQ_ID%

Generalized loop iterators are similar to simple For loop statements in
structure so share the same set of structural coverage requirements.

.. rubric:: Testing Strategy

Generalized loop iterators provide direct access to components or flexible
means to run over iterator objects when the runtime profile allow it.

We verify compliance with the For loop construct requirements using a group of
testcases checking the tool behavior on For-Of constructs iterating over a
variety of array kinds:

.. qmlink:: TCIndexImporter

   *
