Overall organization of the GNATcoverage qualification test harness

--------
Overview
--------

We qualify a well identified GNATcoverage bundle to perform
structural coverage assessments in accordance with a qualified interface.

Depending on the configuration, the tests execution is performed either
through GNATemulator via "gnatcov run" or on a hardware board with specialized
runner and an extra trace conversion step by "gnatcov convert".

The qualification focuses on the ``--annotate=report`` text output of
GNATcoverage, which exposes a list of violations with respect to a given
coverage criterion, such as ``statement not executed at <file>:<line>:<col>``.

To specify how the tool should behave and demonstrate that it produces
reliable outputs, we provide:

* An explicit description of the expected behavior as a set of *Tool
  Operational Requirements* (TORs) for nominal use conditions, and

* A set of executable *Test Cases* (TCs) associated with each requirement,
  exercised to validate that the behavior indeed corresponds to expectations.

We distinguish different categories of tool operational requirements, with
expectations regarding:

* **Core coverage metrics**, for example to validate statement coverage
  assessments on conditional constructs, loops, etc.  Testcases for this kind
  of TORs typically exercise a piece of functional code in various ways, for
  example by causing a Boolean expression to be evaluated just True or False,
  and verify that results are as expected in all the variations.  Programming
  language reference manuals provide a significant contribution in identifying
  the constructs of interest.

* **General coverage analysis facilities**, for example the support for
  coverage exemptions or consolidation capabilities.
  In addition to validating the tool behavior with respect to the stated
  requirements, testcases in this category extend the set of exercised code
  samples where mutliple language features are mixed together.

* **The output report format**, part of the tool qualified interface.
  In addition to dedicated testcases designed to verify that all the mandatory
  pieces are there, part of these requirements are also implicitly validated
  by the execution of all the coverage checking testcases in other categories,
  where specific sections of the report are scanned to search for criteria
  violation messages.

-------------------
TORs and Test Cases
-------------------

Tool Operational Requirements and Test Cases are all collected and linked
together as a tree.

Tool Operational Requirements
*****************************

TORs specify the tool expected behavior in nominal conditions of use.

Each TOR comes with a set of Test Cases to validate it, and every TOR
description comes with a Testing Strategy text describing how the tests are
organized to demonstrate that the tool complies with the requirement.

Requirement and Testcase *Groups* are introduced to expose a general
organisation constructed as a logical hierarchy.

Test Cases
**********

A Test Case is set of Tests aimed at validating part or all of an operational
requirement. We support two categories of Tests:

* Program tests, which run a specific program, perform coverage analysis
  over it and match the analysis outcome against stated expectations,

* Consolidation tests, which perform coverage analysis for the consolidated
  set of traces obtained for a given set of program tests, and match the
  analysis outcome against stated expectations as well.

The sources for a program test always involve two categories of files:

* Functional sources, code for which some coverage properties are to be
  assessed,

* Driver sources, which invoke the functional code in various ways and embed a
  description of the expected coverage outcome.

File names starting with ``test_`` identify driver sources. Multiple drivers
may be used to exercise a single functional source.

Consolidation scenarii are identified as ``cons_<scenario_id>.txt`` text
files.  Each specifies the set of drivers to consolidate followed by the
corresponding coverage expectations.

The following example illustrates a possible set of files involved for a
fictive Test Case exercising an Ada function F which implements an ``and
then`` evaluation. The test case features 4 tests, with three program tests
and one consolidation scenario::

  andthen.adb          -- functional code, exposes
                       --   function F (A, B: Boolean) return Boolean

  test_andthen_tt.adb  -- driver calling F (True, True), and stating what
                       -- coverage results are expected over andthen.adb

  test_andthen_tf.adb  -- driver calling F (True, False)
  test_andthen_fx.adb  -- driver calling F (False, X) for X in [True, False]

  cons_andthen_all.adb -- consolidation spec for all the program tests,
                       -- with the corresponding expected coverage results

As for requirements, Test Case *Groups* are introduced for organizational
purposes as needed.

Coverage Expectations
*********************

The description of expectations on coverage results is achieved with two
devices:

* **In functional sources**, comments starting with ``-- #`` on lines for
  which coverage expectations need to be specified. These provide ways to
  refer to functional lines from ...

* **In driver sources, at the end**, a sequence of:

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
  ``d-``    ``decision never eveluated``               decision coverage
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

Below is a simple example, with a functional ``in_range.adb`` Ada source
first::

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

Then a driver with expectations referencing the functional
lines with markers::

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

* No violation diagnostic for lines 3, 5, and 6, stated by the ``0``
  expectations for the other lines where statements reside.

Which would translate in an expected piece of report output such as::

      2.1. NON-EXEMPTED VIOLATIONS

      in_range.adb:4:7: statement not executed
      in_range.adb:8:7: statement not executed
      2 violations

.. _harness-rationale:

Rationale
*********

The rationale for introducing the embedded expectations circuitry, instead of,
for example, straight file comparisons with pre-recorded expected outputs, is
threefold:

* It brings a lot of flexibility to accomodate minor changes in output
  formatting or line numbers in test cases, which facilitates maintenance;

* It involves developers actively in the expectations specification
  process, which needs to be done very carefully.

* It allows sharing sources across test cases in a very well controlled
  manner, which lets us multiply the number of tests, hence the qualification
  assessment strength, without causing an untractable growth of the testsuite
  complexity.

The inclusion of .xcov results in embedded coverage expectations (even though
this output format is not part of the qualified interface) is motivated by
several factors:

* We need to assess the quality of these outputs during our development
  testing campains, and leveraging the qualification testbase for this purpose
  has clear maintenance benefits.  We don't produce those outputs during
  qualification runs, however, to make sure that they don't interfere with the
  qualification results.

* Having to fill them in reinforces the Test Case development rigor, as it
  adds one element that test writers have to care about when specifying
  expected outcomes.

------------------------
Test evaluation criteria
------------------------

A test either PASSes of FAILs. A test passes if and only if it runs to
completion without hitting any cause of failure. We rely on a few concepts
and mechanisms to validate our tests:

Internal Assertions for Program Tests
*************************************

The general process for every Program Test is to build the program, run it,
produce the corresponding coverage results and check if they correspond to the
expectations stated in the test driver source (see :ref:`Testsuite Engine
<harness-engine>` for more details).

The first possible cause of test FAILure is an unexpected execution
interruption, for example from an uncaught exception occurrence in Ada.

We leverage this to enforce self validation of our testcases thanks to
internal functional assertions, aborting execution as soon as one is not met,
which provides extra confidence that what the test does corresponds to what
was intended by its author.


Match between actual coverage results and stated expectations
*************************************************************

After checking for internal assertions, our testsuite driver expects a strict
one-to-one match between result expectations stated in testcases and the
diagnostics emitted by the tool. On this account, a test PASSes only if:

* Every reported violation has been stated as expected, and

* Every violation stated as expected has been reported.

In other word, any violation reported but not expected or expected but not
reported triggers a test FAILure.

This makes the ``0`` expressions representative of positive coverage
expectations in a context where the qualified output report does not
materialize positive results explicitly.

In the previous example, ``-- /XcmpMax/ l+ ## 0`` is a way to state that we
expect the statement on line 5 (marked with "# XcmpMax") to be covered, and
the testsuite engine verifies this even though the output report does *not*
feature any explicit indication to that effect. Technically, we state that we
expect 0 violation messages on that line, and any violation indication emitted
for it (e.g. if the statement happened not to be covered) would cause the test
to fail.

When a single statement spans over multiple lines, we have situations where we
need to specify expectations for all the lines while there's actually just a
single real positive expectation (as there is only one statement). We use the
``0c`` expectation code in such cases, to indicate that we expect nothing to
be reported for the line (and have testsuite engine check that), but that this
is the continuation of another expectation stated earlier, so shouldn't be
counted as a positive expectation in qualification test-results reports.


Test categories vs. execution level
***********************************

Each testcase is designed to validate a particular TOR, typically associated
with a specific coverage criterion. We have testcases designed to validate
aspects of Statement Coverage assessments, others aimed at Decision Coverage
etc. We call *category* the particular criteria for which a testcase was
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
lines and violations of stricter criteria there ought to be ignored.


More on expectations semantics
******************************

The essential purpose of the qualification process is to make sure that
improperly covered items are reported as such.

On this ground, the testsuite enforces stricter checks for '``!``' and
'``-``' items than for '``+``':

* For '``-``' or '``!``' items, there must be an exact match between the
  stated expectations and results reported by gnatcov (in both output formats
  examined):
  every expectation must be found in the tool outputs, and every occurrence
  in the tool output must have a corresponding expectation.

  This makes sure that expectations are specified carefully and that the
  tool reports exactly what we expect.

* For '``+``' items (.xcov outputs only), only the first of the previously
  described checks applies. Absence of an expectation statement for '``+``' on
  a line doesn't cause a test failure.

``/regexp/`` filters that select no lines are allowed and act as a
no-ops. This is useful in situations where a single driver is shared across
different tests.

Non-empty intersections between different filters are "allowed" as well but
eventhough sometimes convenient, they most often correspond to mistakes. The
sets of expected indications just accumulate.

