.. _testsuite-overview:

Overview of the test procedures organization
********************************************

The GNATcoverage *testsuite* consists of the set of executable *tests* that
implement *Testcases*, driven by an execution and control harness to validate
part or all of a *Tool Operational Requirement*. Each test involves sources
and a description of expected results, which, in association with the harness
operation, consitutes a *test procedure*.

Very broadly, the testsuite harness builds executables from the sources,
executes the resulting programs, invokes GNATcoverage to perform some coverage
analysis assessment and checks that the tool's output matches expectations.
Depending on the configuration, the tests' execution is performed either
through GNATemulator via "gnatcov run", or on a hardware board with
specialized runner and an extra trace conversion step via "gnatcov convert".

Kinds of tests and sources
==========================

The testsuite harness supports two kinds of Tests:

* Program tests, which run a specific program, perform coverage analysis
  over it and match the analysis outcome against stated expectations,

* Consolidation tests, which perform coverage analysis for the consolidated
  set of traces obtained for a given set of program tests, and match the
  analysis outcome against stated expectations as well.

The source files for a program test fall into three categories:

* Functional sources, which embed language constructs on which coverage
  properties are the test purpose.

* Driver sources, which invoke the functional code in various ways and embed a
  description of the expected coverage outcome.

* Helper sources, there only to make the test compilable, irrelevant to the
  exercised operational aspect.

File names starting with ``test_`` identify driver sources. Multiple drivers
may be used to exercise a single functional source. Consolidation scenarios
are identified as ``cons_<scenario_id>.txt`` text files.  Each specifies the
set of drivers to consolidate followed by the corresponding coverage
expectations.

The following example illustrates a possible set of files involved for a
hypothetical Testcase exercising an Ada function F which implements an ``and
then`` evaluation. The test case features four tests, with three program tests
and one consolidation scenario::

  andthen.adb          -- functional code, F (A, B: Boolean) return Boolean

  test_andthen_tt.adb  -- driver calling F (True, True), and stating what
                       -- coverage results are expected over andthen.adb

  test_andthen_tf.adb  -- driver calling F (True, False)
  test_andthen_fx.adb  -- driver calling F (False, X) for X in [True, False]

  cons_andthen_all.adb -- consolidation spec for all the program tests,
                       -- with the corresponding expected coverage results

Coverage Expectations
=====================

Definition and operation
------------------------

The expectations on coverage results are documented with two devices:

* **In functional sources**, comments starting with ``-- #`` on lines for
  which coverage expectations need to be specified. These provide
  a mechanism for referring to functional lines,

* **In driver sources, at the end**, a sequence of comments like:

  | ``--# <functional_file_name>`` followed by an optional sequence of:
  | ``--  /regexp/ <expected .xcov note> ## <expected report notes>`` lines

In the optional sequence at the end of driver sources:

* ``/regexp/`` selects all the source lines matching ``-- # <regexp>``

* ``<expected .xcov note>`` denotes the synthesis sign we expect on the
  selected lines in the ``--annotate=xcov`` output, to be stated even though
  that output is not qualified (see :ref:`rationale <harness-rationale>`):

  =========================  =======================
  Text                       Denotes an expected ...
  =========================  =======================
  ``l-``, ``l+``, or ``l!``  ``-``, ``+``, or ``!``
                             coverage status synthesis sign, respectively

  ``l#``, ``l*``             ``#`` or ``*``
                             exemption status synthesis sign, respectively
  =========================  =======================


* ``<expected report notes>`` is a comma separated sequence of coverage
  violation diagnostics expected for the selected lines in the
  ``--annotate=report`` output:

  =======   ========================================== ==================
  Text      Denotes an expected diagnostic like:       In report section:
  =======   ========================================== ==================
  ``0``     no diagnostic                              all
  ``0c``    no diagnostic, continuation (see below)    all
  ``s-``    ``statement not executed``                 stmt coverage
  ``dT-``   ``decision outcome True not covered``      decision coverage
  ``dF-``   ``decision outcome False not covered``     decision coverage
  ``d!``    ``one decision outcome not covered``       decision coverage
  ``d-``    ``decision never evaluated``               decision coverage
  ``eT-``   ``decision outcome True not covered``      mcdc coverage
  ``eF-``   ``decision outcome False not covered``     mcdc coverage
  ``c!``    ``condition influence not demonstrated``   mcdc coverage
  ``x0``    ``exempted region, 0 exemptions``          exempted regions
  ``x+``    ``exempted region, > 0 exemptions``        exempted regions
  =======   ========================================== ==================


  Some of these notes require precise source location designations, such as a
  line segment to identify a specific condition.

  This is achieved with a ``:"subtext"`` extension to the note, for example
  ``c!:"B"`` to denote the second condition on a line with ``V := A and then
  B;``.

The following example consists of functional code (``in_range.adb``)
and a driver::

    1:  function In_Range (X , Min, Max : Integer) return Boolean is
    2:  begin
    3:     if X < Min then     -- # XcmpMin
    4:        return False;    -- # XoutMin
    5:     elsif X > Max then  -- # XcmpMax
    6:        return False;    -- # XoutMax
    7:     else
    8:        return True;     -- # Xin
    9:     end if;
   10:  end;

The driver code uses markers to specify its expectations for
the referenced lines in the functional code::

      --  Exercise X > max only. Verify that the < min exit and the
      --  in-range case are reported uncovered.

      procedure Test_In_Range_GTmax is
      begin
         Assert (not In_Range (4, 2, 3));
      end;

      --# in_range.adb
      --  /XcmpMin/  l+ ## 0
      --  /XoutMin/  l- ## s-
      --  /XcmpMax/  l+ ## 0
      --  /XoutMax/  l+ ## 0
      --  /Xin/      l- ## s-

The ``<expected .xcov note>`` (2nd) column for ``in_range.adb`` states
that we expect a ``--annotate=xcov`` output with::

      expected notes here
        v
      1 .: function In_Range (X , Min, Max : Integer) return Boolean is
      2 .: begin
      3 +:    if X < Min then     -- # XcmpMin
      4 -:       return False;    -- # XoutMin
      5 +:    elsif X > Max then  -- # XcmpMax
      6 +:       return False;    -- # XoutMax
      7 .:    else
      8 -:       return True;     -- # Xin
      9 .:    end if;
     10 .: end;

The ``<expected report notes>`` (3rd) column indicates what we're expecting
from the qualified output out of ``gnatcov --annotate=report`` :

* ``statement not covered`` diagnostics for lines 4 and 8, stated by the
  ``s-`` expectations, and

* No violation diagnostic for lines 3, 5, and 6, per the ``0``
  expectations for the other lines with statements.

This will yield an expected section of the report output such as::

      2.1. NON-EXEMPTED VIOLATIONS

      in_range.adb:4:7: statement not executed
      in_range.adb:8:7: statement not executed
      2 violations

Extra details on semantics
--------------------------

The essential purpose of the qualification process is to make sure that
improperly covered items are reported as such. For this reason, the testsuite
enforces stricter checks for '``!``' and '``-``' items than for '``+``':

* For '``-``' or '``!``' items, there must be an exact match between the
  stated expectations and results reported by gnatcov (in both output formats
  examined):
  every expectation must be found in the tool outputs, and every occurrence
  in the tool output must have a corresponding expectation.
  This ensures that expectations are specified carefully and that the
  tool reports exactly what is expected.

* For '``+``' items (non-qualified .xcov outputs only), only the first of the
  previously described checks applies. Absence of an expectation statement for
  '``+``' on a line does not cause a test failure.

``/regexp/`` filters that select no lines are allowed and act as a
no-ops. This is useful in situations where a single driver is shared across
different tests. Non-empty intersections between different filters are
"allowed" as well, however most often correspond to mistakes as the sets of
expected indications simply accumulate.

.. _harness-rationale:

Rationale
=========

There are several reasons for introducing the embedded expectations circuitry,
instead of, for example, straight file comparisons with pre-recorded expected
outputs:

* It makes it easier to accomodate minor changes in output
  formatting or line numbers in test cases, which facilitates maintenance;

* It involves developers actively in the expectations specification
  process, which needs to be done very carefully.

* It allows sharing sources across test cases in a well controlled
  manner. This increases the number of tests significantly, and thus
  provides greater confidence in the qualification
  assessment strength, without causing an untractable growth of the testsuite
  complexity.

The inclusion of .xcov results in embedded coverage expectations (even though
this output format is not part of the qualified interface) is motivated by
several factors:

* We need to assess the quality of these outputs during our development
  testing campaigns, and leveraging the qualification testbase for this purpose
  has clear maintenance benefits.  We do not produce those outputs during
  qualification runs, however, and thus they do not interfere with the
  qualification results.

* The need to include this information reinforces the Test Case development
  rigor, since it needs to be taken into account by test writers when they
  specify expected outcomes.

Test evaluation criteria
========================

A test either PASSes of FAILs. A test passes if and only if it runs to
completion without encountering any cause of failure. We rely on a few concepts
and mechanisms to validate the tests:

Internal Assertions for Program Tests
-------------------------------------

The general process for every Program Test is to build the program, run it,
produce the corresponding coverage results and check if they correspond to the
expectations stated in the test driver source.

The first possible cause of test FAILure is an unexpected execution
interruption, for example from an uncaught exception occurrence in Ada.

We use this mechanism to enforce self validation of the testcases through
internal functional assertions, aborting execution as soon as one is not met.
This provides extra confidence that the test's effect is as
intended by its author.


Match between actual coverage results and stated expectations
-------------------------------------------------------------

After checking for internal assertions, the testsuite driver expects a strict
one-to-one match between result expectations stated in testcases and the
diagnostics emitted by the tool. Thus a test PASSes only if:

* Every reported violation has been stated as expected, and

* Every violation stated as expected has been reported.

In other words, any violation reported but not expected or expected but not
reported triggers a test FAILure.

This makes the ``0`` expressions representative of positive coverage
expectations in a context where the qualified output report does not
exhibit positive results explicitly.

In the previous example, ``-- /XcmpMax/ l+ ## 0`` is a way to state that we
expect the statement on line 5 (marked with "# XcmpMax") to be covered, and
the testsuite engine verifies this even though the output report does *not*
feature any explicit indication to that effect. Technically, we state that we
expect 0 violation messages on that line, and any violation indication emitted
for it (e.g. if the statement happened not to be covered) would cause the test
to fail.

When a single statement spans  multiple lines, we have situations where we
need to specify expectations for all the lines while there's actually just a
single real positive expectation (as there is only one statement). We use the
``0c`` expectation code in such cases, to indicate that we expect nothing to
be reported for the line (and have the testsuite engine check that),
but that this is the continuation of another expectation stated earlier,
and thus should not be
counted as a positive expectation in qualification test-results reports.


Test categories vs. execution level
-----------------------------------

Each testcase is designed to validate a particular TOR, typically associated
with a specific coverage criterion. We have testcases designed to validate
aspects of Statement Coverage assessments, others aimed at Decision Coverage
etc. We call *category* the particular criterion for which a testcase was
designed.

Test categories determine the set of potential coverage violations relevant
for each test, which does not necessarily correspond to the set of potential
violations that might be reported for it, in particular when the overall tool
qualification objectives target a stricter criterion.

For example, consider this excerpt of functional code to be exercised for a
Statement Coverage TOR::

  procedure Check (Do_Inner : Boolean) is
  begin
    if Do_Inner then  -- # test
      Inner_Action;   -- # action
    end if
  end;

A single driver that calls into this code with ``Do_Inner = True`` expects to
achieve full statement coverage and would feature ``0`` expectations to convey
that, for example::

  procedure Test_Check is
  begin
    Check (Do_Inner => True);
  end;

  --# check.adb
  --  /test/   l+ ## 0
  --  /action/ l+ ## 0

Now, SC tests also apply when the qualification objectives include, say,
statement + decision coverage, in which case the tool will perform this test
with ``--level=stmt+decision`` and output a decision coverage violation
for the ``# test`` line.
This decision coverage violation is irrelevant for a statement coverage test,
however, and should just be ignored.

In effect, ``0`` expectations need to be (and are) interpreted in accordance
with the test category to prevent FAILures from violations of stricter
criteria. In our example test of statement category, the ``0`` expectations
are meant to convey that we expect no *statement coverage* violation on the
lines, and violations of stricter criteria there ought to be ignored.


