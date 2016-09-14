DC requirements regarding contract based programming facilities
===============================================================

%REQ_ID%

The presence of disabled assertions and Ada2012 contract expressions
shall not influence the coverage assessment on other parts of the program.

.. rubric:: Testing Strategy

We exercise a few subprograms featuring various forms of assertions and
contracts, all disabled, and verify that statement and decision coverage
results on the regular code are those we would expect without the assertions.

.. qmlink:: SubsetIndexImporter

   *


