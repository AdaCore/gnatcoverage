.. _exceptions:

SC requirements for ARM chap. 11 : Exceptions
=============================================


%REQ_ID%

For local exceptions, handled in the same subprogram as where it was raised,
Statement Coverage shall be assessed for Ada's exceptions mechanism as
described in Chapter 11 of the Ada Reference Manual. In particular:

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

The tool shall also produce correct Statement Coverage assessments for
subprogram bodies consisting of a call to subprogram which may raise followed
by simple scalar assignments which never raise, all protected by handlers
which only performs scalar assignments that never raise. Statements never
reached either because the outer subprogram isn't called or because the inner
subprogram raised, shall be reported uncovered.

.. rubric:: Testing Strategy

For local exceptions, we validate the requirements through a set of testcases
that exercise implicit or explicit exceptions for purposes of control flow
transfer. All the tescases follow a common pattern, involving:

* Explicit ``raise`` statements, executed or not, followed by other statements
  or not

* Variations of these in subprograsm or package bodies,
  directly within the top-level sequence of statements, and within nested
  block, conditional or loop statements,

* With one or more candidate handlers at different levels of nesting,
  within a single body.
 
For exceptions that may propagate from a called subprogram as in the
conditions stated in the requirement, we exercise situations where:

* The outer subprogram isn't called at all,

* The inner subprogram is called only in conditions so it raises,

* The inner subprogram is called only in conditions so it does not raise,

* The inner subprogram is called in conditions so it raises and in conditions
  so it does not raise.

.. qmlink:: TCIndexImporter

   *



