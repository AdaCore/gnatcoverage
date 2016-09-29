Check contracts capabilities and transparency when disabled
===========================================================

Check that the presence of disabled assertions and Ada2012 contracts, possibly
involving conditional expressions, does not interfere with the coverage
assessment on other parts of the program.

We exercise a few subprograms featuring various forms of assertions and
contracts, all disabled, and verify that statement and mcdc coverage results
on the function code are those we would expect without the assertions.

Compared to the corresponding Robustness part of the Decision coverage
testbase, here we also check that boolean expressions with multiple operands
in contracts aren't subject to mcdc analysis when the contracts are disabled.

.. qmlink:: SubsetIndexImporter

   *


