SC requirements for Null subprograms
=====================================

%REQ_ID%

Null procedures, completion or a definition in a declarative part, shall be
treated as regular statements, diagnosed uncovered if the procedure is not
called.

Calls to null suprobgrams shall be treated as regular call statements, diagnosed
uncovered if not executed.

.. rubric:: Testing Strategy

This requirement is validated by the following set of testcases:


.. qmlink:: TCIndexImporter

   *



