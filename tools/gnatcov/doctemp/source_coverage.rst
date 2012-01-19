************************
Source Coverage Analysis
************************

General principles & Compilation requirements
=============================================

Source coverage analysis computes metrics focused on source programming
language entities such as high level `statements` or `decisions` (DO178
parlance for boolean expressions).

For this purpose, |gcp| relies on :term:`Source Coverage Obligation` (SCO)
tables, compact descriptions of the nature and source location of program
entities relevant to source coverage criteria.  These tables are part of the
Library Information produced by the |gpro| compilers, in the .ali or .gli file
corresponding to each Ada or C unit, respectively.

Source coverage obligations are produced on demand, by the :option:`-gnateS`
compilation option for Ada, and by the :option:`-fdump-scos` option for
C. These options must be used to compile the sources you wish to analyze later
on. In addition, all the sources must also be compiled :option:`-g`
:option:`-fpreserve-control-flow`, both necessary to allow an accurate mapping
of the execution traces back to source level obligations. Optimization is
supported up to :option:`-O1`, with inlining allowed.

Once your application is built, the analysis proceeds in two steps: |gcvrun|
is used to produce execution traces, then |gcvcov| to generate coverage
reports. *Source* coverage, in particular, is queried by passing a specific
:option:`--level` argument. The possible values for source level analysis are
``stmt``, ``stmt+decision`` and variants of ``stmt+mcdc``, all described in detail
in later sections of this documentation.

The compiler output is suitable whatever the assessed criteria; there is never
a requirement to recompile just because a different criterion needs to be
analyzed.

The :ref:`gnatcov_run-commandline` section of this document provides details on
the trace production interface. The remainder of this chapter explains the use
of |gcvcov| in particular, to analyse traces once they have been produced.


The following sections now describe the available report formats, then
provide more details and examples regarding the supported coverage criteria.

.. _sreport-formats:

Output report formats
=====================

Source coverage reports may be produced in various formats, as requested with
the :option:`--annotate` command line argument of |gcvcov|.

The :option:`xcov` and :option:`html` formats both produce a set of annotated
source files, in the directory where |gcv| is launched unless overriden with
a :option:`--output-dir` option.

The :option:`report` output consists in a synthetic text report of
:term:`coverage violations` with respect to the requested criteria, produced on
standard output by default or in the file specified by the :option:`-o`
command line option.

Later in this documentation we name output formats by the text to add to
:option:`--annotate` on the command line. For example, we use "the
:option:`=report` outputs" to mean "the coverage reports produced with
:option:`--annotate=report` ".

In all the cases, the report focuses on the sources stated to be of interest
by way of the :option:`--scos` command line argument.

Annotated sources, text : :option:`--annotate=xcov[+]`
------------------------------------------------------

For source coverage criteria, |gcvcov| :option:`--annotate=xcov` produces an
annotated version of each source file, in text format, named after the original
source with an extra ``.xcov`` extension at the end.

Each annotated source contains a global summary of the assessment results
followed by the original source lines, all numbered and marked with a coverage
annotation next to the line number. The annotation on a line always consists
in a single character, which may be one of the following:

.. csv-table::
   :delim: |
   :widths: 10, 80
   :header: Annotation, Meaning

   ``.`` | No coverage obligation is attached to the line
   ``+`` | Coverage obligations attached to the line, all satisfied
   ``-`` | Coverage obligations attached to the line, none satisfied
   ``!`` | Coverage obligations attached to the line, some satisfied

To illustrate, let us consider that we exercise our example functional unit in
the following fashion, with ``X1 < X2`` in every call:

::

  procedure Test_X1X2 is
  begin
     Assert (Between (X1 => 2, X2 => 5, X => 3)); -- X1 < X < X2
     Assert (not Between (X1 => 2, X2 => 5, X => 8)); -- X1 < X2 < X
  end;

This executes the ``if`` statement twice, evaluates the controlling decision
True only and executes the first ``return`` statement twice, to return True
then False.

If we then perform, say, Statement Coverage analysis, we get a ``+``
annotation for the corresponding lines, a ``-`` for the line with the second
``return`` statement (never executed), and a ``.`` everywhere else.

Here is the full report produced for our example unit, where the ``Between``
function is actually part of an Ada package abstraction. The original source
file is ``range.adb`` so the annotated version is ``range.adb.xcov``:

::

 gnatcov/examples/docsupport/src/ranges.adb:
 67% of 3 lines covered
 Coverage level: stmt
   1 .: package body Ranges is
   2 .:
   3 .:    function Between (X1, X2, X : Integer) return Boolean is
   4 .:    begin
   5 +:       if X1 < X2 then
   6 +:          return X >= X1 and then X <= X2;
   7 .:       else
   8 -:          return X >= X2 and then X <= X1;
   9 .:       end if;
  10 .:    end;
  11 .:
  12 .: end;

:option:`--annotate=xcov+` (with a trailing +) works the same, only providing
extra details below lines with improperly satisfied obligations. The available
details consists in the list of coverage :term:`violations` diagnosed for the
line, which depends on the coverage criteria involved. Here is an excerpt for
our previous example, where the only improperly satisfied obligation is an
uncovered statement on line 8:

::

 ...
   8 -:          return X >= X2 and then X <= X1;
   STATEMENT "return X ..." at 8:10 not executed
 ...

Annotated sources, html : :option:`--annotate=html[+]`
------------------------------------------------------

For source coverage criteria, |gcvcov| :option:`--annotate=html` produces an
annotated version of each source file, in html format, named after the original
source with an extra ``.html`` extension at the end.

Each annotated source page contains a summary of the assessment results
followed by the original source lines, all numbered and marked with a coverage
annotation as in the :option:`--annotate=xcov` case. In addition, lines with
obligations are colorized in green, orange or red for ``+``, ``!`` or ``-``
coverage respectively.

An `index.html` page is also produced, which contains a description of the
assessment context (assessed criteria, set of trace files involved, ...) and a
summary of the coverage results for all the units, with links to their
annotated sources.

See our :ref:`sample html index <sample_sc_html_index>` appendix for an
example index page, which embeds a self-description of all the items it
contains. See the :ref:`sample annotated source <sample_sc_html_unit>`
appendix for a sample of html annotated source.

Similarily to the :option:`xcov` format case, :option:`--annotate=html+` (with
a trailing +) adds details about improperly satisfied obligations.  In the
html version, these extra details are not immediatly visible: they are folded
within their associated line and expanded when a mouse click hits the line.

Violations summary, text : :option:`--annotate=report`
------------------------------------------------------

For source coverage criteria, |gcvcov| :option:`--annotate=report` produces a
syntetic text report that lists all the :term:`coverage violations` (failure
to satisfy some aspect of a coverage criterion) relevant to the set of
assessed criteria.

The report features explicit start/end of report notifications and
at least three sections in between: Assessment Context, Coverage Violations,
and Analysis Summary.  The general structure is sketched below and a more
detailed description of each report section follows.

::

  ** COVERAGE REPORT **

  ===========================
  == 1. ASSESSMENT CONTEXT ==
  ===========================
  ...
  ============================
  == 2. COVERAGE VIOLATIONS ==
  ============================
  ...
  =========================
  == 3. ANALYSIS SUMMARY ==
  =========================
  ...
  ** END OF REPORT **


A few variations are introduced when :term:`exemption regions` are in scope.
See the :ref:`exemptions` section for more details on their use and effect on
the output reports.

Assessment Context
^^^^^^^^^^^^^^^^^^

The *Assessment Context* report section exposes the following information
items:

* Date & time when the report was produced
* Command line and Version of |gcp| that produced the report
* Coverage level requested to be analyzed
* Details on the input trace files:
  path to binary program exercised (as provided on the command line),
  production time stamp and tag string (:option:`--tag` command line
  argument value).

Here is a example excerpt:

::

  ===========================
  == 1. ASSESSMENT CONTEXT ==
  ===========================

  Date and time of execution: 2011-11-24 16:33:44.00
  Tool version: XCOV 1.0.0w (20111119)

  Command line:

  gnatcov coverage --scos=@eng.alis --level=stmt+mcdc --annotate=report t0.trace

  Coverage level: stmt+mcdc

  Trace files:

  t0.trace
    program: obj/powerpc-elf/test_engines
    date   : 2011-11-24 15:33:44
    tag    : sample run


The set of units that this report is about is conveyed by the
:option:`--scos` option arguments on the quoted command line.

Coverage Violations
^^^^^^^^^^^^^^^^^^^

The *Coverage Violations* report section lists and counts the coverage
violations that relate to source lines not part of an exemption region.  The
violations are grouped in subsections, one per assessed criterion according to
the :option:`--level` option:

.. csv-table::
   :delim: |
   :header: :option:`--level=`, Assessed criteria / Report subsections
   :widths: 10, 50

   `stmt`          | Statement Coverage
   `stmt+decision` | Statement and Decision Coverage
   `stmt+mcdc`     | Statement, Decision and MCDC Coverage


All the violations are reported using a consistent
format, as follows:

::

    queues.adb:1641:17: statement not executed
     (source) : (loc) : (violation description)


*source* and *loc* are the basename of the source file and
the precise ``line:column`` location within that source where the
violation was detected.

The following table summarizes the list of violation items that
might be emitted together for each criterion:

.. csv-table::
   :delim: |
   :widths: 30 65
   :header: Criterion, Possible violations

   Statement Coverage | ``statement not executed``
   Decision Coverage  | ``decision outcome TRUE not covered``
                      | ``decision outcome FALSE not covered``
                      | ``one decision outcome not covered``
   MCDC Coverage      | all the decision coverage items, plus ...
                      | ``condition has no independent influence pair``


When multiple violations apply someplace, the most salliant diagnostic is
emitted alone. For instance, if an Ada statement like ``X := A and then B;``
is not covered at all, a ``statement not executed`` violation is emitted
alone, even if we're assessing for, say, :option:`--level=stmt+decision` ;
|gcv| emits no decision oriented violation in this case.

Here is an output excerpt for our example with :option:`--level=stmt+mcdc`,
producing one subsection for each of the three criteria requested at that
level:

::

  ============================
  == 2. COVERAGE VIOLATIONS ==
  ============================

  2.1. STMT COVERAGE
  ------------------

  ranges.adb:8:10: statement not executed

  1 violation.

  2.2. DECISION COVERAGE
  ----------------------

  ranges.adb:5:10: decision outcome FALSE never exercised

  1 violation.

  2.3. MCDC COVERAGE
  ------------------

  ranges.adb:6:17: condition has no independent influence pair, MC/DC not achieved

  1 violation.

Analysis Summary
^^^^^^^^^^^^^^^^

The *Analysis Summary* report section summarizes just the counts reported in
each of the previous sections.  For our example report so far, this would be:

::

  =========================
  == 3. ANALYSIS SUMMARY ==
  =========================

  1 non-exempted STMT violation.
  1 non-exempted DECISION violation.
  1 non-exempted MCDC violations.


This section provides a quick way to determine whether the requested coverage
level is fully satisfied, with details available from the per criterion
sections that precede.


Statement Coverage (SC) assessments
===================================

General principles
------------------

Statement Coverage analysis, which focuses on :dfn:`statement` source
entities. is requested with the :option:`--level=stmt` command line
argument of |gcvcov|.

In synthetic :option:`=report` outputs, unexecuted statements are reported as
Statement Coverage violations in the report section dedicated to these.

In annotated source outputs, the coverage annotations convey the following
indications:

.. csv-table::
   :delim: |
   :widths: 10, 80
   :header: Annotation, Meaning

   ``+`` | At least one statement on the line, all covered
   ``-`` | At least one statement on the line, none covered
   ``!`` | At least one statement on the line, some covered

When a single statement spans multiple lines, the coverage annotation is
present on all the lines, as the two ``+`` signs for the single assignment
in the following excerpt::

  2 .:  -- A single assignment spanning two lines
  3 .:
  4 +:  Result :=
  5 +:     Input1 + Input2;
  6 .:

For compound statements, the coverage status of the compound construct per se
is reported only on the parts that embed flow control expressions. For an Ada
*if* statement, for example, coverage is reported on the ``if`` or ``elsif``
lines only, not on the ``else``, or ``end if;`` lines, and not on lines where
inner statements reside. The lines where inner statements reside are annotated
in accordance with the nature and coverage status of those statements
only. For example, see the ``.`` annotations on lines 4 and 6 in::

  2 +:  if This_Might_Not_Be_True then
  3 -:     Result := -1;
  4 .:  else
  5 +:     Result := 12;
  6 .:  end if;

Declarations are generally considered as statements, so are reported
covered/uncovered when they have initialization code associated with them.

Finally, a statement is considered covered as soon as part of the associated
machine code is executed, in particular even when the statement execution is
interrupted somehow, for example by an exception occurrence. For instance, the
statement below::

  X := Function_That_Raises_Exception (Y) + Z;

Will be reported as covered as soon as it is reached, even if the expression
evaluation never really terminates.


Example program and assessments
-------------------------------

To illustrate the just presented points further, we consider the example
functional unit below, with the spec and body stored in source files named
``div_with_check.ads`` and ``div_with_check.adb``::

   function Div_With_Check (X, Y : Integer) return Integer;
   --  If Y /= 0, divide X by Y and return the result. Raise
   --  Program_Error otherwise.

   function Div_With_Check (X, Y : Integer) return Integer is
   begin
      if Y = 0 then
         raise Program_Error;
      else
         return X / Y;
      end if;
   end;

We first exercise the function for Y = 1 only, using a
the following :term:`test driver` in ``test_div1.adb``::

   procedure Test_Div1  is
      X : constant Integer := 4;
   begin
      Assert (Div_With_Check (X, 1) = X);
   end;


Once the driver+application bundle is built, we have a ``test_div1``
executable that we execute with::

  gnatcov run test_div1

This produces ``test_div1.trace``, which we analyze for the Statement Coverage
criterion as follows::

  gnatcov coverage --level=stmt --scos=div_with_check.ali --annotate=xcov test_div1.trace

Since we pass a single :option:`--scos` argument with a straight ``.ali`` file
name, the analysis focuses on the corresponding unit alone. Results for the
test drivers and harness are most often not of interest because these units
are not part of the applicative code for which coverage objectives are to be
met.

:option:`--annotate=xcov` requests results as annotated sources in text format,
which we get in ``div_with_check.adb.xcov``::

   docsupport/src/div_with_check.adb:
   67% of 3 lines covered
   Coverage level: stmt
      1 .: function Div_With_Check (X, Y : Integer) return Integer is
      2 .: begin
      3 +:    if Y = 0 then
      4 -:       raise Program_Error;
      5 .:    else
      6 +:       return X / Y;
      7 .:    end if;
      8 .: end;

We can observe that:

- Only the ``if`` line of the compound *if* statement is annotated,
  as covered since the function was called.

- The inner ``raise`` and ``return`` statements are marked uncovered and
  covered respectively, as expected since the function was only called with
  arguments for which the ``if`` controling decision evaluates False.

As a second experiment, we exercise the function for Y = 0 only, using a the
following :term:`test driver` in ``test_div0.adb``::

   procedure Test_Div0  is
      Result : Integer
        := Div_With_Check (4, 0);
   begin
      Put_Line ("R = " & Integer'Image (Result));
   end;

The analysis proceeds in a very similar fashion as the previous one. We
request results on the test driver as well this time, as it features
constructs relevant to the points we wish to illustrate::

  ls test_div0.ali div_with_checks.ali > alis
  gnatcov coverage --level=stmt --scos=@alis --annotate=xcov test_div0.trace

The first command is a Unix-like way to create a file named ``alis`` which
contains the list of ALI files corresponding to the units we want included in
the assessement results.

The :option:`=xcov` outputs we obtain follow. First, results for the
functional unit, with the ``if`` statement coverage reversed::

   docsupport/src/div_with_check.adb:
   67% of 3 lines covered
   Coverage level: stmt
      1 .: function Div_With_Check (X, Y : Integer) return Integer is
      2 .: begin
      3 +:    if Y = 0 then
      4 +:       raise Program_Error;
      5 .:    else
      6 -:       return X / Y;
      7 .:    end if;
      8 .: end;
      9 .:

Then, results for the test driver where we can note that

- The two lines of the local ``Result`` definition are annotated,

- This definition is marked covered even though it was evaluated only once
  with an initialization expression that raised an exception, and

- The driver body is reported uncovered, as expected since an exception
  triggered during the elaboration of the subprogram declarative part.

::

   docsupport/src/test_div0.adb:
   67% of 3 lines covered
   Coverage level: stmt
      1 .: with Div_With_Check, Ada.Text_IO; use Ada.Text_IO;
      2 .:
      3 .: procedure Test_Div0  is
      4 +:    Result : Integer
      5 +:      := Div_With_Check (4, 0);
      6 .: begin
      7 -:    Put_Line ("R = " & Integer'Image (Result));
      8 .: end;

The corresponding synthetic report is simply obtained by running |gcvcov|
again with :option:`--annotate=report` instead of :option:`--annotate=xcov`::

   ** COVERAGE REPORT **

   ===========================
   == 1. ASSESSMENT CONTEXT ==
   ===========================

   Date and time of execution: 2012-01-11 16:37:17.00
   Tool version: XCOV 1.0.0w (20081119)

   Command line:

   gnatcov coverage --level=stmt --scos=@alis --annotate=report test_div0.trace

   Coverage level: stmt

   Trace files:

   test_div0.trace
     program: obj/test_div0
     date   : 2012-01-11 15:37:17
     tag    :

   ============================
   == 2. COVERAGE VIOLATIONS ==
   ============================

   2.1. STMT COVERAGE
   ------------------

   div_with_check.adb:6:7: statement not executed
   test_div0.adb:7:4: statement not executed

   2 violations.

   =========================
   == 3. ANALYSIS SUMMARY ==
   =========================

   2 STMT violations.

   ** END OF REPORT **

We can see here that the two lines marked ``-`` in the :option:`=xcov` outputs
are properly reported as violations in the ``STMT COVERAGE`` section of this
report, and that this section is the only one presented in the ``COVERAGE
VIOLATIONS`` part, as only this criterion was to be analyzed per the
:option:`--level=stmt` argument.

Decision Coverage (DC) assessments
==================================

General principles
------------------

|gcv| performs combined Statement and Decision Coverage assessments
with the :option:`--level=stmt+decision` command line option.

In this context, we consider to be :dfn:`decisions` all the Boolean
expressions used to influence the control flow via explicit constructs in the
source program, such as ``if`` statements or ``while`` loops. For proper
operation, only short-circuit operators are allowed to combine operands, as
enforced by the `No_Direct_Boolean_Operator` restriction pragma offered by the
|gnat| compilers for Ada.

A decision is said :dfn:`fully covered`, or just :dfn:`covered`, as soon as it
has been evaluated at least once True and once False during the program
execution. If only one or none of these two possible outcomes was exercised,
the decision is said :dfn:`partially covered`.  The case where none of the
possible outcomes was exercised happens when the enclosing statement was not
executed at all, or when all the attempted evaluations were interrupted
e.g. because of exceptions.

The following table summarizes the meaning of the :option:`=xcov` and
:option:`=html` annotations:

.. csv-table::
  :delim: |
  :widths: 10, 80
  :header: Annotation, Meaning

   ``+`` | All the statements and decisions on the line are covered
   ``-`` | Statement on the line was not executed
   ``!`` | At least one decision partially covered on the line


When a trailing `+` added the annotation format passed to :option:`--annotate`
(so with :option:`=xcov+` or :option:`=html+`), a precise description of the
actual violations available for each line in addition to the annotation.

The :option:`=report` synthetic output lists the statement and decision
coverage violations, in the ``STMT`` and ``DECISION`` coverage report section
respectively.

Whatever the format, when a decision is part of a statement and the statement
is uncovered, only the statement level violation is reported. The nested
decision level violations are implicit in this case and diagnosing them as
well would only add redundancy.

Example program and assessments
-------------------------------

To illustrate the just presented points, we consider the example functional
Ada unit below, with the spec and body stored in source files named
``divmod.ads`` and ``divmod.adb``::

   procedure Divmod
     (X, Y : Integer; Value : out Integer;
      Divides : out Boolean; Tell : Boolean);
   --  Compute X / Y into VALUE and set DIVIDES to indicate whether
   --  Y divides X. Output a note to this effect when requested to TELL.

   procedure Divmod
     (X, Y : Integer; Value : out Integer;
      Divides : out Boolean; Tell : Boolean) is
   begin
      if X mod Y = 0 then
         Divides := True;
         if Tell then
            Put_Line (Integer'Image (Y) & " divides " & Integer'Image (X));
         end if;
      else
         Divides := False;
      end if;

      Value := X / Y;
   end Divmod;

We first experiment with the following test driver::

   procedure Test_Divmod2  is
      Value : Integer;
      Divides : Boolean;
   begin
      Divmod (X => 5, Y => 2, Value => Value,
              Divides => Divides, Tell => True);
      Assert (Divides = False);

      Divmod (X => 6, Y => 2, Value => Value,
              Divides => Divides, Tell => True);
      Assert (Divides = True);
   end Test_Divmod2;

This exercises the ``Divmod`` function twice. The outer ``if`` construct
executes both ways and the ``if Tell then`` test runs once only for ``Tell``
True. As a result, the only :option:`stmt+decision` violation by our driver is
the ``Tell`` decision coverage, only partially achieved since we have only
exercised the True case. This is confirmed by the section of :option:`=report`
output that follows, where we find the two coverage violations sections
expected for the requested set of criteria::

   2.1. STMT COVERAGE
   ------------------

   No violation.

   2.2. DECISION COVERAGE
   ----------------------

   divmod.adb:14:10: decision outcome FALSE never exercised

   1 violation.

For :option:`--annotate=xcov`, this translates as follows::

   8 .: procedure Divmod
   9 .:   (X, Y : Integer; Value : out Integer;
  10 .:    Divides : out Boolean; Tell : Boolean) is
  11 .: begin
  12 +:    if X mod Y = 0 then
  13 +:       Divides := True;
  14 !:       if Tell then
  15 +:          Put_Line (Integer'Image (Y) & " divides " & Integer'Image (X));
  16 .:       end if;
  17 .:    else
  18 +:       Divides := False;
  19 .:    end if;
  20 .:
  21 +:    Value := X / Y;
  22 .: end Divmod;

Now we exercise with another test driver::

   procedure Test_Divmod0  is
      Value : Integer;
      Divides : Boolean;
   begin
      Divmod (X => 5, Y => 0, Value => Value,
              Divides => Divides, Tell => True);
   end Test_Divmod0;

Here we issue a single call passing 0 for the Y argument, which triggers a
check failure for the ``mod`` operation. This results in the following
:option:`=xcov` output::

   8 .: procedure Divmod
   9 .:   (X, Y : Integer; Value : out Integer;
  10 .:    Divides : out Boolean; Tell : Boolean) is
  11 .: begin
  12 !:    if X mod Y = 0 then
  13 -:       Divides := True;
  14 -:       if Tell then
  15 -:          Put_Line (Integer'Image (Y) & " divides " & Integer'Image (X));
  16 .:       end if;
  17 .:    else
  18 -:       Divides := False;
  19 .:    end if;
  20 .:
  21 -:    Value := X / Y;
  22 .: end Divmod;

We have an interesting situation where

* While the outer ``if`` statement is reached and covered (as a statement),
  the ``X mod Y = 0`` embedded decision is actually never evaluated because
  the only evaluation attempted is interrupted by an exception.

* None of the other statements is ever reached as a result.

This gets all confirmed by the :option:`=report` output below, on which we
also notice that the only diagnostic emitted for the uncovered inner ``if`` is
the statement coverage violation::

   2.1. STMT COVERAGE
   ------------------

   divmod.adb:13:7: statement not executed
   divmod.adb:14:7: statement not executed
   divmod.adb:15:10: statement not executed
   divmod.adb:18:7: statement not executed
   divmod.adb:21:4: statement not executed

   5 violations.

   2.2. DECISION COVERAGE
   ----------------------

   divmod.adb:12:7: decision never evaluated

   1 violation.


Modified Condition/Decision Coverage (MCDC) assessments
=======================================================

In a similar fashion to statement or decision coverage, |gcv| features
Modified Condition/Decision Coverage assessment capabilities with
*--level=stmt+mcdc*.
In addition to the particular level specification, you should also
provide |gcvrun| with the set of SCOs you plan to analyze later on
using the produced trace, with a `--scos` argument as for
`gnatcov coverage`.
If you plan different analysis for a single run, providing a common
superset to |gcvrun| is fine.
Providing |gcvrun| with only a subset of the SCOs you will analyze
might result in pessimistic assessments later on (spurious MCDC not
achieved outcome).

To support MCDC, we introduce a distinction between two kinds of
Boolean expressions:


* @dfn:term:`Simple` Boolean expressions are Boolean atoms such as a lone
  Boolean variable or a function call, possibly negated.

* @dfn:term:`Complex`
  Boolean expressions are those that feature at least two Boolean atoms
  combined with short-circuit operators, the only ones allowed for
  proper operation as for Decision Coverage.


In addition to simple and complex expressions used to influence
control-flow statements, we treat as decisions all the complex Boolean
expressions anywhere they might appear.
For example, the Ada code excerpt below:


::

    X := A and then not B;
    if Y then [...]


... features two expressions subject to MCDC analysis: `A and then not B` (complex expression with two atoms), on the right hand
side of the assignment to `X`, and the simple `Y` expression
that controls the `if` statement.
The Boolean atoms in a decision are called @dfn:term:`conditions` in the
DO-178 literature.  The types involved need not be restricted to the
standard Boolean type when one is defined by the language; For Ada,
typically, they may subtypes or types derived from the fundamental
Boolean type.

Compared to Decision Coverage, MCDC assessments incur extra
verifications on the demonstration by the tests of the independent
influence of conditions on decisions.
Several variants of the criterion exist, with a common idea: for each
condition in a decision, tests are required to expose a pair of
valuations where both the condition and the decision value change
while some extra property on the other conditions holds.
The point is to demonstrate that every condition is significant in the
decision and that the tests exercised representative combinations of
the possible behaviors, while keeping the number of required tests
linear with the number of conditions in a decision.

@dfn:term:`Unique Cause MCDC` is a common variant where the extra property
is 'all of the other conditions in the decision shall remain unchanged'.
To illustrate, the table below expands the 4 possible
condition/decision vectors for decision `A and then B`.
`T`/`F` represent the True/False boolean values and the
rightmost column indicates which vector pairs demonstrate Unique Cause
independent effect of each condition.


::

  | # | A  B  A && B | Indep |
  |---|--------------|-------|
  | 1 | T  T    T    | A  B  |
  | 2 | T  F    F    |    B  |
  | 3 | F  T    F    | A     |
  | 4 | F  F    F    |       |



|gcp| actually implements a common variant, accepting variations of
other conditions in an independence pair as long as they could for
sure not possibly influence the decision outcome, e.g. due to
short-circuit semantics.
This variant, well known as @dfn:term:`Masking` MCDC @bibref:term:`ar018`,
@bibref:term:`cast6` provides additional flexibility on the set of tests
required to satisfy the criterion without reducing the minimal size of
this set.
In the `and then` case, it becomes possible to use the #4 + #1
pair as well to demonstrate the independent influence of `A`, as
`B` is not evaluated at all when `A` is False so the change
on `B` is irrelevant in the decision switch.

Output-wise, the in-source notes for the `xcov` or `html`
formats are the same as for decision coverage reports, with condition
specific cases marked with '!' as well.
`--annotate=report` outputs feature specific diagnostics where
conditions are identified with their precise file:line:column source
location.
Using the same decision as in the previous example to illustrate, we
run the Explore robot in Cautious mode only, try both safe and unsafe
actions and get:


::

  robots.adb:75:10: condition has no independent influence pair, MC/DC not achieved


Such condition related messages are only emitted when no more general
diagnostic applies on the associated decision or statement, however.
In our familiar example, attempting only safe actions in Cautious mode
yields a '`decision outcome TRUE never exercised`' diagnostic,
not a couple of condition related messages.

