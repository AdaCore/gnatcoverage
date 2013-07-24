SC expectations for ARM chap. 11 : Exceptions
==============================================


.. rubric:: Requirement(s)



Statement Coverage shall be assessed for Ada's exceptions mechanism as
described in Chapter 11 of the Ada Reference Manual. In
particular:

* ``raise`` statements shall be reported uncovered when unreached,

* The control flow effects of implicit and explicit exception raises shall be
  handled correctly:

  * statements that do not execute because of a raise shall be reported
    as uncovered,

  * statements that only execute partially because of an interrupted expression
    evaluation shall *not* be reported as uncovered.

* Statement coverage requirements apply to both predefined and
  user-defined exceptions.

* The full set of core SC requirements apply to all the statements within
  exception handlers.



.. rubric:: Testing Strategy



We validate the requirements through a set of testcases that exercise
implicit or explicit exceptions for purposes of control flow transfer.
All the tescases follow a common pattern, involving:

* Explicit ``raise`` statements, executed or not, followed by other statements
  or not

* Variations of these in subprograsm or package bodies,
  directly within the top-level sequence of statements, and within nested
  block, conditional or loop statements,

* With one or more candidate handlers at different levels of nesting,
  within a single body.
 

.. qmlink:: TCIndexImporter

   *



