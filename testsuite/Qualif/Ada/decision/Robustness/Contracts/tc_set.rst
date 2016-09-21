Check contracts capabilities and transparency when disabled
===========================================================

Check that the presence of disabled assertions and Ada2012 contracts, possibly
involving conditional expressions, does not influence the coverage assessment
on other parts of the program.

We exercise a few subprograms featuring various forms of assertions and
contracts, all disabled, and verify that statement and decision coverage
results on the regular code are those we would expect without the assertions.

.. qmlink:: SubsetIndexImporter

   *


