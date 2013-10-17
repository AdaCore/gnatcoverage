SC requirements for Return statements
=====================================


.. rubric:: Requirement(s)



A RETURN statement is both a statement per se and a control flow
trigger. 

* A RETURN statement shall be reported as uncovered when it is not executed,

* When a RETURN statement is executed, all the statements in the subprogram
  body that have not yet been executed shall be reported as uncovered.


.. rubric:: Testing Strategy



We exercise subprograms containing one or more return statements in
various contexts, conditional or not, with several combinations
of the possible execution flow variants.


.. qmlink:: TCIndexImporter

   *


