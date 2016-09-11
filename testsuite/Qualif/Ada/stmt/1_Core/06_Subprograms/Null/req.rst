SC requirements for Null subprograms
=====================================

%REQ_ID%

The null statement attached to a null procedure, be it a completion or a
definition in a declarative part, shall be treated as a regular statement,
diagnosed uncovered if and only if the procedure is not called.


.. rubric:: Testing Strategy

This requirement is validated by the following set of testcases:


.. qmlink:: TCIndexImporter

   *



