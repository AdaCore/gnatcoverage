SC requirements for No_Return pragmas on suprograms
===================================================


%REQ_ID%

SC assessments shall not be influenced by the presence of a No_Return pragma
attached to a subprogram. In particular, calls to such subprograms, statements
within such subprograms or within subprograms called by them shall be
diagnosed as not-executed if and only if they are not reached by the actual
program control flow.

.. rubric:: Testing Strategy

This requirement is validated by the following set of testcases:


.. qmlink:: TCIndexImporter

   *



