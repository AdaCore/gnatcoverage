SC expectations for ARM chap. 11 : Exceptions
=============================================

SC expectations regarding ARM chap. 11 : Exceptions


Requirement(s)
--------------



Statement Coverage shall be assessed correctly for Ada exceptions mechanism as
described in Chapter 11 "Exceptions" of the Ada Reference Manual. In
particular:

* ``raise`` statements shall be reported uncovered when unreached,

* The flow-control effects of implicit and explicit exception raises shall be
  handled correctly:

  * statements that don't execute because of a raise shall be reported
    uncovered,

  * statements that only execute partially because of an expression
    evaluation interrupted shall *not* be reported uncovered.

* The tool shall support user defined exceptions as well as language
  predefined ones,

* The full set of core SC requirements apply to all the statements within
  exception handlers.



Testing Strategy
----------------



We validate all those requirements through a set of testcases that resort to
implicit or explicit exceptions for flow-control transfer purposes. All these
tescases obey a common testing variation pattern all along; with checks that
involve:

* Explicit ``raise`` statements executed or not, followed by other statements
  or not

* Variations of these in function, subprogram, or package elaboration bodies,
  directly within the toplevel sequence of statements, within nested block,
  conditional or loop statements,

* With one or more candidate handlers at different levels of nesting, always
  within a single body.
 

.. qmlink:: TCIndexImporter

   *



