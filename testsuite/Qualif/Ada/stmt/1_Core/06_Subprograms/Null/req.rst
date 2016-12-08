SC requirements for Null subprograms
=====================================

%REQ_ID%

The null statement attached to a null procedure, as a completion or part of a
definition, shall be treated as a regular statement, diagnosed uncovered if
the procedure is not called.

Calls to null suprobgrams shall be treated as regular call statements, diagnosed
uncovered if not executed.

.. rubric:: Testing Strategy

This requirement is validated by the following set of testcases:


.. qmlink:: TCIndexImporter

   *
