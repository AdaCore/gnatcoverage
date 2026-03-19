.. _exceptions:

SC requirements for ARM chap. 11 : Exceptions
=============================================


Statement Coverage assessments shall handle the Ada exceptions related
features described in Chapter 11 of the language Reference Manual. This
requires accounting for exception handlers, implicit or explicit raise
operations and the effects of their control flow transfers. More precisely:

======  ======================================================================
Rule #  Description
======  ======================================================================
1       Explicit ``raise`` statements shall be handled as regular
        statements, reported uncovered when not reached,

2       Statements never reached in a sequence because of implicit or explicit
        raise(s) earlier in the sequence shall be reported uncovered,

3       The complete set of Statement Coverage requirements apply to statements
        that constitute exception handlers,

4       Statements that execute only partially, interrupted by a raise
        during an expression evaluation as part of the statement's operation,
        shall *not* be reported as uncovered.

5       Predefined and user-defined exceptions shall be handled in the same
        manner.
======  ======================================================================

.. rubric:: Testing Strategy

We first validate all the rules in contexts where handlers are within the
subprogram where the exceptions get raised, known as *local handers*. The set
of testcases exercise implicit or explicit exceptions for purposes of control
flow transfer, involving:

* Explicit ``raise`` statements, executed or not, followed by other statements
  or not,

* Variations of these in subprograms or package bodies,
  directly within the top-level sequence of statements, and within nested
  block, conditional or loop statements,

* With one or more candidate handlers, possibly at different levels of
  block nesting within a single function or procedure body.

.. qmlink:: TCIndexImporter

   LocalHandlers


Rules #1, 2 and 5 are checked by variations in most testcases and verified
more specifically by the *Raise* testcase, dedicated to this purpose.

Rule #3 is verified by the *Within* family of testcases, which exercise
conditional constructs within exception handlers in a variety of contexts
where such handlers may appear.

Rule #4 is verified by the dedicated *CutEvals* set of testcases.

When conditional guards are involved, the coverage outcome is always checked
at least for situations where a guard is tested True only and False only. Very
often, testcases also verify the outcome when a guard is tested both True and
False or not at all.

For exceptions that propagate across subprograms, we first verify all
the requirement rules in a single *Basics* testcase, then check more
elaborate situations with a family of tests involving:

* Various exception kinds (implicit standard exception raised from a
  language check, explicit raise of standard or user defined exception,
  raise from a Ada runtime facility);

* That propagate either directly or via an intermediate handler and
  a "raise;" statement propagating the original exception further, from
  either a specific handler for the particular exception or from a
  "when others" handler;

* That are eventually either unhandled or handled in a variety of contexts,
  for example within a library level subprogram or within a handler for
  another exception occurrence.

.. qmlink:: TCIndexImporter

   Propagation
