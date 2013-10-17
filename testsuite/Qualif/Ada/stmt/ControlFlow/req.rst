SC requirements for statement sequences and control-flow transfers
==================================================================

.. rubric:: Requirement

Statement coverage shall be assessed correctly for straightline sequences
(basic blocks) of statements, and combinations of such using jump-like control
flow transfers (goto, return, raise, exit).

Check the following:

* If execution reaches the top of a basic block, no statement in the
  block is reported as uncovered. 

* Inversely, if execution does not reach the top of a basic block, 
  all the statements in the block shall be reported uncovered.


.. rubric:: Testing Strategy



The following set of testcases verifies compliance with this requirement:
 

.. qmlink:: TCIndexImporter

   *



