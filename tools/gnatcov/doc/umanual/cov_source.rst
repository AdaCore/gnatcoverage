.. _scov:

************************
Source Coverage Analysis
************************

.. _scov-principles:

General principles & Compilation requirements
=============================================

Source coverage analysis computes metrics focused on source programming
language entities such as high level `statements` or `decisions` (DO178
parlance for boolean expressions). In |gcp| terms, we designate these entities
as :term:`Source Coverage Obligations`, or SCOs. SCO tables, describing the
nature and source location of each item of interest, are part of the
information produced by the |gpro| compilers, in the .ali or .gli
:term:`Library Information file` corresponding to each Ada or C unit.

SCO tables are produced by the :option:`-fdump-scos` compilation
option. Accurate mapping of the execution traces back to source level
obligations requires :option:`-g` :option:`-fpreserve-control-flow` in
addition, as well as :option:`-gno-strict-dwarf` for VxWorks targets. These
options must be used to compile the sources you wish to analyze later
on. Optimization is supported up to :option:`-O1` with inlining.

Once your application is built, the analysis proceeds in two steps: |gcvrun|
is used to produce execution traces, then |gcvcov| to generate coverage
reports. *Source* coverage is queried by passing a specific :option:`--level`
argument to |gcvcov|. The compiler output is suitable whatever the assessed
criteria; there is never a requirement to recompile just because a different
criterion needs to be analyzed.

The :ref:`gnatcov_run-commandline` section of this document provides details
on the trace production interface. The remainder of this chapter focuses on
the use of |gcvcov| to analyse traces once they have been produced.  The
general structure of this command line is always like::

  gnatcov coverage --level=<criterion> --annotate=<format>
                   <unit selection arguments> ... <traces>

The next sections describe the :ref:`available report formats
<sreport-formats>`, then provide more details regarding :ref:`scov-stmt`,
:ref:`scov-dc`, and :ref:`scov-mcdc`. The :ref:`sunits` section describes how
application units to be considered for coverage assessment are to be
specified.  Essentially, this is achieved by command line options that
designate the sets of relevant Source Coverage Obligations, either straight
from Library Information files with the :option:`--scos` option, or leveraging
the higher level GNAT project file support with a :option:`-P` option.

.. _sreport-formats:

Output report formats (:option:`--annotate`)
============================================

Source coverage reports may be produced in various formats, as requested with
the :option:`--annotate` command line argument of |gcvcov|. The
:option:`xcov`, :option:`html` and :option:`dhtml` formats produce a set of
annotated source files, in the directory where |gcv| is launched unless
overriden with a :ref:`--output-dir option <cov-outdir>`. The :option:`report`
output consists in a synthetic text report of :term:`coverage violations` with
respect to the requested criteria, produced on standard output by default or
in the file specified by the :option:`-o` command line option.

Later in this chapter we name output formats by the text to add to
:option:`--annotate` on the command line. For example, we use "the
:option:`=report` outputs" to mean "the coverage reports produced with
:option:`--annotate=report`".

We will illustrate the various formats with samples extracted from outputs
obtained by perfoming coverage analysis of the following example Ada
application unit::

   function Between (X1, X2, V : Integer) return Boolean;
   --  Whether V is between X1 and X2, inclusive and regardless
   --  of their ordering.

   function Between (X1, X2, V : Integer) return Boolean is
   begin
      if X1 < X2 then
         return V >= X1 and then V <= X2;
      else
         return V >= X2 and then V <= X1;
      end if;
   end Between;

Annotated sources, text (:option:`=xcov[+]`)
--------------------------------------------

For source coverage criteria, |gcvcov| :option:`--annotate=xcov` produces an
annotated version of each source file, in text format, named after the
original source with an extra ``.xcov`` extension at the end (``x.ext.xcov``
for a source named ``x.ext``).

Each annotated source contains a global summary of the assessment results
followed by the original source lines, all numbered and marked with a coverage
annotation next to the line number. The annotation on a line always consists
in a single character, which may be one of the following:

.. tabularcolumns:: cl
.. csv-table::
   :delim: |
   :widths: 10, 80
   :header: Annotation, Meaning

   ``.`` | No coverage obligation is attached to the line
   ``-`` | Coverage obligations attached to the line, none satisfied
   ``!`` | Coverage obligations attached to the line, some satisfied
   ``+`` | Coverage obligations attached to the line, all satisfied

Here is, to illustrate, the full statement coverage report produced for our
example unit when the ``Between`` function was called so that the ``if``
control evaluated True only. The function is actually part of an Ada package,
called Ranges, with an original body source file named ``ranges.adb``:

.. code-block:: ada

 examples/src/ranges.adb:
 67% of 3 lines covered
 Coverage level: stmt
   1 .: package body Ranges is
   2 .:    function Between (X1, X2, V : Integer) return Boolean is
   3 .:    begin
   4 +:       if X1 < X2 then
   5 +:          return V >= X1 and then V <= X2;
   6 .:       else
   7 -:          return V >= X2 and then V <= X1;
   8 .:       end if;
   9 .:    end;
  10 .: end;

:option:`--annotate=xcov+` (with a trailing +) works the same, only providing
extra details below lines with improperly satisfied obligations. The available
details consists in the list of :term:`coverage violations` diagnosed for the
line, which depends on the coverage criteria involved. Here is an excerpt for
our previous example, where the only improperly satisfied obligation is an
uncovered statement on line 7::
 
   7 -:          return V >= X2 and then V <= X1;
   STATEMENT "return V ..." at 7:10 not executed

Annotated sources, html (:option:`=html[+]`)
--------------------------------------------

For source coverage criteria, |gcvcov| :option:`--annotate=html` produces an
annotated version of each source file, in html format, named after the
original source with an extra ``.html`` extension at the end.  Each annotated
source page contains a summary of the assessment results followed by the
original source lines, all numbered and marked with a coverage annotation as
in the :option:`--annotate=xcov` case. Lines with obligations are colorized in
green, orange or red for ``+``, ``!`` or ``-`` coverage respectively. 

An `index.html` page is also produced, which contains a summary of the
assessment context (assessed criteria, trace files involved, ...) and of the
coverage results for all the units, with links to their annotated sources.
See our :ref:`sample html index <sample_sc_html_index>` appendix for an
example index page, which embeds a self-description of all the items it
contains. See the :ref:`sample annotated source <sample_sc_html_unit>`
appendix for a sample of html annotated source.

The page style is governed by a set of Cascading Style Sheet (CSS) parameters,
fetched from a ``xcov.css`` file in the directory where |gcv| is launched. If
this file is available when |gcv| starts, |gcv| uses it so users may setup a
customized version if needed. If the file is not available, |gcv| creates a
default one.

Similarily to the :option:`xcov` format case, :option:`--annotate=html+` (with
a trailing +) adds details about improperly satisfied obligations.  In the
html version, these extra details are initially folded within their associated
line and expanded by a mouse click on the line.

Annotated sources, dynamic html (:option:`=dhtml`)
-------------------------------------------------

:option:`--annotate=dhtml` produces a *dynamic html* output, which essentially
features:

* A more modern look & feel compared to the :option:`html` formats described
  earlier,

* The ability to sort indexes by clicking on column headers, allowing for
  example sorts keyed on unit names or on relative coverage achievement,

* Per-project indexes on the root page when :option:`-P` was used to designate
  the source units of interest.

The option produces a set of `.js` javascript files implementing most of the
report displays and interactions, as well as an `index.html` root page which
users should open as an entry point to the report contents.

The per-line details that differentiates :option:`html+` from :option:`html`
are always produced, initially folded and available on line clicks as well.

Violations summary, text (:option:`=report`)
--------------------------------------------

For source coverage criteria, |gcvcov| :option:`--annotate=report` produces a
summary that lists all the :term:`coverage violations` (failure
to satisfy some aspect of a coverage criterion) relevant to the set of
assessed criteria.

The report features explicit start/end of report notifications and at least
three sections in between: Assessment Context, Coverage Violations, and
Analysis Summary.  A few variations are introduced when :term:`exemption
regions` are in scope.  See the :ref:`exemptions` section for more details on
their use and effect on the output reports.

Assessment Context
^^^^^^^^^^^^^^^^^^
The *Assessment Context* report section exposes the following information
items:

* Date & time when the report was produced

* Command line and Version of |gcp| that produced the report. The set of units
  that the report is about is conveyed by the command line switches
  summarized there (:option:`--projects`, :option:`--units`, :option:`--scos`).

* Coverage level requested to be analyzed

* Details on the input trace files:
  path to binary program exercised (as provided on the command line),
  production time stamp and tag string (:option:`--tag` command line
  argument value).

Here is a example excerpt::

  ===========================
  == 1. ASSESSMENT CONTEXT ==
  ===========================

  Date and time of execution: 2011-11-24 16:33:44.00
  Tool version: GNATcoverage 1.0.0w (20111119)

  Command line:

  gnatcov coverage -Pmytest.gpr --level=stmt+mcdc --annotate=report test_x1x2.trace

  Coverage level: stmt+mcdc

  Trace files:

  test_x1x2.trace
    program: obj/test_x1x2
    date   : 2011-11-24 15:33:44
    tag    : sample run


Coverage Violations
^^^^^^^^^^^^^^^^^^^

The *Coverage Violations* report section lists and counts the coverage
violations that relate to source lines not part of an exemption region.  The
violations are grouped in subsections, one per assessed criterion according to
the :option:`--level` option:

.. tabularcolumns:: ll
.. csv-table::
   :delim: |
   :header: :option:`--level=`, Assessed criteria / Report subsections
   :widths: 10, 50

   `stmt`          | Statement Coverage
   `stmt+decision` | Statement and Decision Coverage
   `stmt+mcdc`     | Statement, Decision and MCDC Coverage


All the violations are reported using a consistent
format, as follows::

    ranges.adb:7:10: statement not executed
      source  :sloc: violation description

*source* and *sloc* are the source file basename and the precise
``line:column`` location within that source where the violation was detected.

The following table summarizes the list of violation items that
might be emitted together for each criterion:

.. tabularcolumns:: ll
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


When multiple violations apply someplace, only the most basic diagnostic is
emitted, not the more precise ones corresponding to stricter criteria. For
instance, if an Ada statement like ``X := A and then B;`` is not covered at
all, a ``statement not executed`` violation is always emitted alone, even when
assessing :option:`--level=stmt+mcdc` and we also have improper decision and
conditions coverage.

Here is an output excerpt for our example with :option:`--level=stmt+mcdc`,
producing one subsection for each of the three criteria requested at that
level::

  ============================
  == 2. COVERAGE VIOLATIONS ==
  ============================

  2.1. STMT COVERAGE
  ------------------

  ranges.adb:7:10: statement not executed

  1 violation.

  2.2. DECISION COVERAGE
  ----------------------

  ranges.adb:4:10: decision outcome FALSE never exercised

  1 violation.

  2.3. MCDC COVERAGE
  ------------------

  ranges.adb:5:17: condition has no independent influence pair, MC/DC not achieved

  1 violation.

Analysis Summary
^^^^^^^^^^^^^^^^

The *Analysis Summary* report section summarizes just the counts reported in
each of the previous sections.  For our example report so far, this would be::

  =========================
  == 3. ANALYSIS SUMMARY ==
  =========================

  1 non-exempted STMT violation.
  1 non-exempted DECISION violation.
  1 non-exempted MCDC violations.


This section provides a quick way to determine whether the requested coverage
level is fully satisfied, with details available from the per criterion
sections that precede.


.. _scov-stmt:

Statement Coverage analysis (:option:`--level=stmt`)
=======================================================

|gcv| performs Statement Coverage assessments with the :option:`--level=stmt`
command line option.

In synthetic :option:`=report` outputs, unexecuted source statements are
listed as Statement Coverage violations in the report section dedicated to
these.

In annotated source outputs, the coverage annotations convey the following
indications:

.. tabularcolumns:: cl
.. csv-table::
   :delim: |
   :widths: 10, 80
   :header: Annotation, Meaning

   ``-`` | At least one statement on the line, none covered
   ``!`` | At least one statement on the line, some covered
   ``+`` | At least one statement on the line, all covered

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
``div_with_check.ads`` and ``div_with_check.adb``:

.. code-block:: ada

   function Div_With_Check (X, Y : Integer) return Integer;
   --  return X / Y if Y /= 0. Raise Program_Error otherwise

   function Div_With_Check (X, Y : Integer) return Integer is
   begin
      if Y = 0 then
         raise Program_Error;
      else
         return X / Y;
      end if;
   end;

We first exercise the function for Y = 1 only, using
the following :term:`test driver` in ``test_div1.adb``:

.. code-block:: ada

   procedure Test_Div1  is
      X : constant Integer := 4;
   begin
      Assert (Div_With_Check (X, 1) = X);
   end;

From a ``test_div1.trace`` obtained with |gcvrun|, we analyze for the
Statement Coverage criterion using the following |gcvcov| invocation::

  gnatcov coverage --level=stmt --scos=div_with_check.ali --annotate=xcov test_div1.trace

We get an :option:`=xcov` annotated source result in text format for the
functional unit on which the analysis is focused, in
``div_with_check.adb.xcov``::

    examples/src/div_with_check.adb:
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

As a second experiment, we exercise the function for Y = 0 only, using:

.. code-block:: ada

   procedure Test_Div0  is
      Result : Integer
        := Div_With_Check (4, 0);
   begin
      Put_Line ("R = " & Integer'Image (Result));
   end;

We request results on the test driver as well this time, as it features
constructs of relevance for our purposes::

  gnatcov coverage --level=stmt -Pmytest.gpr --annotate=xcov test_div0.trace

The :option:`=xcov` outputs follow. First, for the functional unit, with the
``if`` statement coverage reversed compared to the previous testcase::

      1 .: function Div_With_Check (X, Y : Integer) return Integer is
      2 .: begin
      3 +:    if Y = 0 then
      4 +:       raise Program_Error;
      5 .:    else
      6 -:       return X / Y;
      7 .:    end if;
      8 .: end;
      9 .:

Then, for the test driver where we can note that

- The two lines of the local ``Result`` definition are annotated,

- This definition is marked covered even though it was evaluated only once
  with an initialization expression that raised an exception, and

- The driver body is reported uncovered, as expected since an exception
  triggered during the elaboration of the subprogram declarative part.

::

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

   ===========================
   == 1. ASSESSMENT CONTEXT ==
   ===========================
   ...
  
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

We can see here that the two lines marked ``-`` in the :option:`=xcov` outputs
are properly reported as violations in the ``STMT COVERAGE`` section of this
report, and that this section is the only one presented in the ``COVERAGE
VIOLATIONS`` part, as only this criterion was to be analyzed per the
:option:`--level=stmt` argument.

.. _scov-dc:

Decision Coverage analysis (:option:`--level=stmt+decision`)
============================================================

With the :option:`--level=stmt+decision` command line option, |gcv| performs
combined Statement and Decision Coverage assessments.

In this context, we consider to be a :dfn:`decision` any Boolean expression
used to influence the control flow via explicit constructs in the source
program, such as ``if`` statements or ``while`` loops, regardless of the type
of this expression. This may be of essentially any type in C, and subtypes or
types derived from the fundamental Boolean type in Ada.

A decision is said :dfn:`fully covered`, or just :dfn:`covered`, as soon as it
has been evaluated at least once True and once False during the program
execution. If only one of these two possible outcomes was exercised, the
decision is said :dfn:`partially covered`.

A decision is also said :dfn:`partially covered` when none of the possible
outcomes was exercised, which happens when the enclosing statement was not
executed at all or when all the attempted evaluations were interrupted
e.g. because of exceptions. In the former case, when a decision is part of a
statement and the statement is not executed at all, only the statement level
violation is reported. The nested decision level violations are implicit in
this case and diagnosing them as well would only add redundancy.

The :option:`=report` synthetic output lists the statement and decision
coverage violations in the ``STMT`` and ``DECISION`` coverage report section
respectively.

For the :option:`=xcov` and :option:`=html` annotated-source oriented formats,
the single annotation produced on each source line combines the statement and
decision coverage indications. The following table summarizes the meaning of
the possible annotations:


.. tabularcolumns:: cl
.. csv-table::
  :delim: |
  :widths: 10, 80
  :header: Annotation, Meaning

   ``-`` | Statement on the line was not executed
   ``!`` | At least one decision partially covered on the line
   ``+`` | All the statements and decisions on the line are covered


When a trailing `+` is added to the format passed
to :option:`--annotate` (:option:`=xcov+` or :option:`=html+`), a precise
description of the actual violations is available for each line in addition to
the annotation.

Example program and assessments
-------------------------------

To illustrate, we consider the example functional Ada unit below, with the
spec and body stored in source files named ``divmod.ads`` and ``divmod.adb``:

.. code-block:: ada

   procedure Divmod
     (X, Y : Integer; Value : out Integer;
      Divides : out Boolean; Tell : Boolean);
   --  Compute X / Y into VALUE and set DIVIDES to indicate
   --  whether  Y divides X. Output a note to this effect when
   --  requested to TELL.

   procedure Divmod
     (X, Y : Integer; Value : out Integer;
      Divides : out Boolean; Tell : Boolean) is
   begin
      if X mod Y = 0 then
         Divides := True;
         if Tell then
            Put_Line (Integer'Image(Y) & " divides " & Integer'Image(X));
         end if;
      else
         Divides := False;
      end if;

      Value := X / Y;
   end Divmod;

We first experiment with the following test driver:

.. code-block:: ada

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
exercised the True case. This is confirmed by :option:`=report` excerpt below,
where we find the two violations sections in accordance with the requested set
of criteria::

   2.1. STMT COVERAGE
   ------------------

   No violation.

   2.2. DECISION COVERAGE
   ----------------------

   divmod.adb:14:10: decision outcome FALSE never exercised

   1 violation.

For :option:`--annotate=xcov`, this translates as a single
partial coverage annotation on the inner ``if`` control line::

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

Now we exercise with another test driver:

.. code-block:: ada

   procedure Test_Divmod0  is
      Value : Integer;
      Divides : Boolean;
   begin
      Divmod (X => 5, Y => 0, Value => Value,
              Divides => Divides, Tell => True);
   end Test_Divmod0;

Here we issue a single call passing 0 for the Y argument, which triggers a
check failure for the ``mod`` operation.

This results in the following :option:`=xcov` output::

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

We have an interesting situation here, where

* The outer ``if`` statement is reached and covered (as a statement),

* No evaluation of the ``X mod Y = 0`` decision terminates, because the only
  attempted computation is interrupted by an exception, so none of the other
  statements is ever reached.

This gets all confirmed by the :option:`=report` output below, on which we
also notice that the only diagnostic emitted for the uncovered inner ``if``
on line 14 is the statement coverage violation::

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

.. _scov-mcdc:

Modified Condition/Decision Coverage analysis (:option:`--level=stmt+mcdc`)
===========================================================================

|gcv| performs combined Statement and Modified Condition/Decision Coverage
analysis with the :option:`--level=stmt+mcdc` option passed to |gcvcov|. In
addition to this particular :option:`--level` option, you also need to tell
|gcvrun| the list of units on which MCDC analysis will be performed.  See
the :ref:`trace-control` section for more details on this aspect of the
procedure.

Compared to Decision Coverage, MCDC analysis incurs two important differences:

* In addition to expressions that pilot an explicit control-flow construct, we
  treat as decisions all the Boolean expressions that combine operands with
  short-circuit logical operators, such as the expression on the right hand
  side of the assignment in ``X := A and then B;`` More details on the
  identification of decisions, together with extra examples, are provided in
  the :ref:`mcdc-decisions` section of this chapter

* For each decision in the sources of interest, testing shall demonstrate the
  :dfn:`independant influence` of every operand (:term:`conditions` in the
  DO-178 parlance) in addition to just exercising the True/False outcomes of
  the expression as a whole. The :ref:`mcdc-variants` section that follows
  expands on the notion of :dfn:`independant influence` and on possible
  variations of the MCDC criterion definition.

Output-wise, the source annotations for the :option:`=xcov` or :option:`=html`
formats are the same as for decision coverage, with condition specific cases
marked with a ``!`` as well:

.. tabularcolumns:: cl
.. csv-table::
  :delim: |
  :widths: 10, 80
  :header: Annotation, Meaning

   ``-`` | Statement on the line was not executed
   ``!`` | At least one decision/condition partially covered on the line
   ``+`` | All the statements and decisions/conditions on the line are covered


The :option:`=report` outputs feature an extra MCDC section in the Coverage
Violations segment, which holds:

- The condition specific diagnosics (``independent influence not
  demonstrated``), as well as

- Decision level diagnostics (such as ``decision outcome True not covered``
  messages) for the Complex Boolean Expressions not directing a control-flow
  oriented statement and which we treat as decisions nevertheless.

There again, condition or decision related messages are only emitted when no
more general diagnostic applies on the associated entity. Condition specific
diagnostics, for example, are only produced in absence of enclosing statement
or decision level violation.

See the :ref:`mcdc-examples` section of this chapter for a few illustrations
of these points.

.. _mcdc-variants:

MCDC variants
-------------

Compared to Decision Coverage, achieving MCDC requires tests that demonstrate
the independent influence of conditions in decisions. Several variants of the
criterion exist.

:dfn:`Unique Cause MCDC` is the original criterion described in the DO178B
reference guidelines, where independent influence of a specific condition must
be demonstrated by a pair of tests where only that condition changes and the
decision value toggles.

Consider the following table which exposes the 4 possible condition/decision
vectors for the ``A and then B`` expression, where T stands for True, F stands
for False, and the italics indicate that the condition evaluation is
short-circuited:

.. tabularcolumns:: |c|cc|c|
.. csv-table::
   :delim: |
   :header: #, A, B, A and then B

   1 | T | T | T
   2 | T | F | F
   3 | F | *T* | F
   4 | F | *F* | F


Each line in such a table is called an :term:`evaluation vector`, and
the pairs that demonstrate the independant effect of conditions are known as
:term:`independence pairs`.

Evaluations 1 + 3 constitute a Unique Cause independence pair for A, where A
changes, B does not, and the expression value toggles. 1 + 2 constitues a pair
for B.

The closest criterion supported by |gcp| is a very minor variation where
conditions that are not evaluated due to short-circuit semantics are allowed
to differ as well in a pair. Indeed, their value change cannot possibly have
influenced the decision toggle (since they are not even considered in the
computation), so they can never invalidate the effect of another condition.

We call this variation :dfn:`Unique Cause + Short-Circuit MCDC`, activated
with :option:`--level=stmt+uc_mcdc` on the command line. From the ``A and then
B`` table just introduced, 4 + 1 becomes another valid independence pair for
A, as `B` is not evaluated at all when `A` is False so the change on `B` is
irrelevant in the decision switch.

:option:`--level=stmt+mcdc` actually implements another variant, known as
:dfn:`Masking MCDC`, accepted as a sound alternative and offering improved
support for coupled conditions.

Masking MCDC allows even further flexibility in the possible variations of
conditions in an independence pair. Indeed, as soon as only short-circuit
operators are involved, all the conditions that appear on the left of a given
condition in the expression text are allowed to change without invalidating
the said condition influence demonstration by a pair.

.. _mcdc-examples:

Example program and assessments
-------------------------------

We reuse one of our previous examples to illustrate, with a simple functional
unit to exercise:

.. code-block:: ada

   function Between (X1, X2, V : Integer) return Boolean;
   --  Whether V is between X1 and X2, inclusive and however they are ordered

   function Between (X1, X2, V : Integer) return Boolean is
   begin
      if X1 < X2 then
         return V >= X1 and then V <= X2;
      else
         return V >= X2 and then V <= X1;
      end if;
   end Between;

First consider the following test driver, which exercises only a
single case where X1 < V < X2:

.. code-block:: ada

   procedure Test_X1VX2 is
   begin
      Assert (Between (X1 => 2, X2 => 5, V => 3)); -- X1 < V < X2
   end Test_X1VX2;

Performing MCDC analysis requires the execution step to be told about it,
by providing both the :option:`--level` and a list of units for which analysis
is to be performed to |gcvrun| (see the :ref:`trace-control` for details)::

   gnatcov run --level=stmt+mcdc -Pmytest.gpr test_x1vx2

We first request an :option:`=xcov+` report to get a first set of results, in
the ``ranges.adb.xcov`` annotated source::

   gnatcov coverage --level=stmt+mcdc -Pmytest.gpr --annotate=xcov+ test_x1vx2.trace

   ...
      8 .:    function Between (X1, X2, V : Integer) return Boolean is
      9 .:    begin
     10 !:       if X1 < X2 then
   DECISION "X1 < X2" at 10:10 outcome FALSE never exercised
     11 !:          return V >= X1 and then V <= X2;
   DECISION "V >= X1 a..." at 11:17 outcome FALSE never exercised
     12 .:       else
     13 -:          return V >= X2 and then V <= X1;
   STATEMENT "return V ..." at 13:10 not executed
     14 .:       end if;
     15 .:    end Between;

This is all as expected from what the driver does, with a few points of note:

- The diagnostic on line 11 confirms that Complex Boolean Expression are
  treated as decisions even when not used to direct a conditional control-flow
  statement. The expression is indeed used here as a straight, unconditional
  ``return`` statement value;

- Only the decision level violations are emitted for lines 10 and 11. The
  independant influence of the conditions is not demonstrated but this is
  implicit from the decision partial coverage so is not notified;

- Similarily, only the statement level violation is emitted for line 13,
  eventhough there are decision and condition level violations as well.

Another aspect of interest is that we have partial decision coverage on two
kinds of decisions (one control-flow decision controling the *if*, and another
one used a straight return value), and this distinction places the two
``decision outcome FALSE never exercised`` violations in distinct sections of
the :option:`=report` output::


   2.1. STMT COVERAGE
   ------------------
   ranges.adb:13:10: statement not executed

   2.2. DECISION COVERAGE
   ----------------------
   ranges.adb:10:10: decision outcome FALSE never exercised

   2.3. MCDC COVERAGE
   ------------------
   ranges.adb:11:17: decision outcome FALSE never exercised


Now running another test driver which exercises two cases where X1 < X2:

.. code-block:: ada

   procedure Test_X1VX2V is
   begin
      Assert (Between (X1 => 2, X2 => 5, V => 3)); -- X1 < V < X2
      Assert (not Between (X1 => 2, X2 => 5, V => 8)); -- X1 < X2 < V
   end;

The first return expression is valued both ways so we get an example of
*condition* specific diagnostic on line 11::

     8 .:    function Between (X1, X2, V : Integer) return Boolean is
     9 .:    begin
    10 !:       if X1 < X2 then
  DECISION "X1 < X2" at 10:10 outcome FALSE never exercised
    11 !:          return V >= X1 and then V <= X2;
  CONDITION "V >= X1" at 11:17 has no independent influence pair, MC/DC not achieved
    ...

Indeed, looking at an evaluation table for the first return decision:

.. tabularcolumns:: |c|cc|c|c|

.. csv-table::
   :delim: |
   :header: #, A: V >= X1, B: V <= X2, A and then B, Case

   1 | T | T | T | X1 < V < X2
   2 | T | F | F | X1 < X2 < V
   3 | F | *T* | F |
   4 | F | *F* | F |

We observe that our driver exercises vectors 1 and 2 only, where:

- The two evaluations toggle the decision and the second condition only, so
  achieve decision coverage and demonstrate that condition's independant
  influence;

- The first condition (V >= X1) never varies so the independant influence
  of this condition isn't demonstrated.

As we mentioned in the discussion on MCDC variants, adding vector 3
would achieve MCDC for this decision. Just looking at the table,
adding vector 4 instead would achieve MCDC as well since the second
condition is short-circuited so its value change is not relevant. The
condition expressions are such that running vector 4 is not possible,
however, since we can't have V both < X1 (condition 1 False) and V >
X2 (condition 2 False) at the same time when X1 < X2.

.. _mcdc-decisions:

Decision composition rules for MCDC
-----------------------------------

For MCDC analysis purposes, we treat as decisions two categories of
expressions:

- As for the :dfn:`decision coverage` criterion, all the expressions
  that directly influence control-flow constructs and which we will call
  :dfn:`control-flow expressions`,

- All the expressions obtained by composition of short-circuit logical
  operators, ``and-then`` and ``or-else`` for Ada, ``&&`` and ``||`` for C.

The most straightforward examples of non control-flow expressions treated as
decisions for MCDC are the logical expressions appearing in contexts such as
the right-hand side of assignments. For example::

  Valid_Data, Sensor_OK, Last_Update_OK : Boolean;
  ...
  Valid_Data := Sensor_OK and then Last_Update_OK; -- 1 decision here, in Ada

  bool needs_update;
  struct sensor_state * state;
  ...
  needs_update = (state && !state->valid);   /* 1 decision here, in C */

Non short-circuit binary operators, when allowed by the coding standard, are
taken as regular computational devices and may either participate in the
construction of operands or split an expression into multiple decisions. For
instance::

  bool aligned (int x, y)
  ...
    return !(x & 0x3) && !(y & 0x3);

in C produces a single decision with two bitwise ``&`` operands.
And the following Ada excerpt::

  A, B, C, D, E : Boolean;
  ...
  if ((A and then not B) == (C or else (D and then E)))

produces three decisions: ``(A and then not B)``, 2 operands combined with
short-circuit ``and then``, ``(C or else (D and then E)))``, 3 operands
combined with short-circuit ``and then`` and ``or else``, and the whole
toplevel expression as it is controlling an ``if`` statement.

In C as in Ada, logical negation is allowed anywhere and just participates in
the operands construction without influencing decision boundaries.

Non short-circuit binary operators in logical expressions might complexify the
identification of decision boundaries for users. To alleviate this, |gnat|
compilers offer the ``No_Direct_Boolean_Operator`` restriction pragma to
support coding standards prohibiting the use of such operators in Ada. There
is no equivalent in C, where the allowed operand types are much more varied
and where the restriction would make the language really much harder to use.

.. _sunits:

Specifying the units of interest
================================

This section describes the command line switches that can be passed to
|gcvrun| and |gcvcov| to convey the set of units on which coverage should be
assessed, which we will call the set of :dfn:`units of interest`.

.. _passing_scos:

Low-level interface (:option:`--scos`)
--------------------------------------

With the :option:`--scos` command line argument, users convey the set of units
of interest by providing the set of Library Information files corresponding to
those units.

For Ada programs, the GNAT toolchain provides a useful device for this
computation: the :option:`-A` command line argument to :command:`gnatbind`
which produces a list of all the .ali files involved in an executable
construction.  By default, the list goes to standard output. It may be
directed to a file on request with :option:`-A=<list-filename>`, and users may
of course filter this list as you see fit depending on your analysis purposes.
Below is an example sequence of commands to illustrate, using the standard
Unix ``grep`` tool to filter out test harness units, assuming a basic naming
convention::

    # Build executable and produce the corresponding list of ALI files. Pass
    # -A to gnatbind through gprbuild -bargs then filter out the test units:

    gprbuild -p --target=powerpc-elf --RTS=zfp-prep -Pmy.gpr
     test_divmod0.adb -fdump-scos -g -fpreserve-control-flow -bargs -A=all.alis

    grep -v 'test_[^/]*.ali' all.alis > divmod0.alis

    # Run/Analyse using the list of interest:

    gnatcov run --level=stmt+mcdc --scos=@divmod0.alis
    gnatcov coverage --level=stmt+mcdc --annotate=xcov --scos=@divmod0.alis

Each occurrence of :option:`--scos` on the command line expects a single
argument which specifies a set of units of interest. Multiple occurrences are
allowed and the sets accumulate. The argument might be either a single unit
name or a :term:`@listfile argument` expected to contain a list of unit names.

For example, focusing on Ada units ``u1``, ``u2`` and ``u3`` can be achieved
with either ``--scos=u1.ali --scos=u2.ali --scos=u3.ali``, with
``--scos=u3.ali --scos=@lst12``, provided a ``lst12`` text file containing the
first two ALI file names, or with ``--scos=@lst123`` where ``lst123`` is a text
file containing the three unit names.


.. _passing_gpr:

Using project files
-------------------

As an alternative to manually specifying the complete list of Library
Information files to be loaded, you can use GNAT project files to specify
units of interest and let |gcv| determine automatically the location of these
files.

For starters, a single :dfn:`root project` must be specified using the
:option:`-P` option, then :dfn:`projects of interest`, within the project tree
rooted at the given root, may be specified using :option:`--projects` options.
If :option:`-P` is used alone, without any :option:`--projects`, then units of
interest from the root project itself are considered. With
:option:`--projects` options, only the projects listed by these options are
considered. The root project designated by :option:`-P` is not included in the
scope if it is not listed in the :option:`--projects` set as well.
With a lone :option:`-P` or with :option:`--projects` in addition, projects
imported by the listed ones are also considered recursively if
:option:`--recursive` is used.

The following figures illustrate the effect of various combinations, assuming
an example source tree depicted below:

.. image:: prjtree.*
  :align: center

On this project tree, :ref:`fig-Proot` restricts the analysis to units in the
root project only, and :ref:`fig-Proot-ss_a` allows focusing on the Subsystem
A project only. If the root project is of interest as well, it must be listed
explicitly, as in :ref:`fig-Proot-root-ss_a`.

.. _fig-Proot:
.. figure:: Proot.*
  :align: center

  ``-Proot``

.. _fig-Proot-ss_a:
.. figure:: Proot-ss_a.*
  :align: center

  ``-Proot --projects=subsystem_a``

.. _fig-Proot-root-ss_a:
.. figure:: Proot-root-ss_a.*
  :align: center

  ``-Proot --projects=root --projects=ss_a``

:option:`--recursive` lets you consider all the projects transitevely imported
by the designated ones. For example:

.. _fig-Proot-ss_a-recursive:
.. figure:: Proot-ss_a-recursive.*
  :align: center

  ``-Proot --projects=subsystem_a --recursive``

The :option:`-P` and :option:`--projects` options select *projects* of
interest. Now within each of these projects, *units* of interest can also be
specified, using specific attributes in package ``Coverage`` of project files.
Four attributes are available for this purpose: ``Units``, ``Units_List``,
``Excluded_Units``, and ``Excluded_Units_List``.

``Units`` and ``Units_List`` are used to construct an initial set of units for
which coverage analysis should be performed.  Similarily to ``Sources`` and
``Sources_List``, ``Units`` specifies a set of units, and ``Units_List``
specifies the name of a text file containing a list of units. If neither is
specified, then by default all units in the project are considered for
coverage analysis.
For example, given a project with three packages Pak1, Pak2, and Pak3,
if you want to do coverage analysis only for Pak1 and Pak3 you can
specify::

  package Coverage is              or:   package Coverage is
    for Units use ("pak1","pak3");         for Units_List use "units.txt";
  end Coverage;                          end Coverage

``Excluded_Units`` and ``Excluded_Units_List`` operate in a similar way,
indicating units that are never considered for coverage. Back to our example,
the same result as above is obtained by specifying::

   package Coverage is
      for Excluded_Units use ("pak2");  -- Consider all units except "pak2"
   end Coverage;

When the exclude/include sets overlap, the excluding attributes prevail
over the including ones. The exact rules for computation of the units to be
considered for a project are as follows:

- An initial set is determined using the ``Units`` and ``Units_List``
  attributes in the project's ``Coverage`` package; By default, if no such
  attribute is found, the initial set comprises all the units of the project,

- Units determined using the ``Excluded_Units`` and ``Excluded_Units_List``
  attributes are removed from the initial set to yield the set to consider.

The list of units to be considered for a given execution of |gcv| can also be
overridden from the command line using :option:`--units=<UNIT|@LISTFILE>`.
Each occurrence of this switch indicates one unit to focus on, or with the @
syntax the name of a file containing a list of units to focus on. When
:option:`--units` is used, the ``Units``, ``Units_List``, ``Excluded_Units``,
and ``Excluded_Units_List`` attributes are ignored.


Inlining & Ada Generic Units
============================

In the vast majority of situations, inlining is just transparent to source
coverage metrics: calls are treated as regular statements, and coverage of
the inlined bodies is reported on the corresponding sources regardless of
their actual inlining status.

By default, generic units are also uniformly treated as single source
entities, with the coverage achieved by all the instances combined and
reported against the generic source only, not for each individual instance.

Consider the following functional Ada generic unit for example:

.. code-block:: ada

   generic
      type Num_T is range <>;
   package Genpos is
      procedure Count (X : Num_T);
      --  Increment N_Positive is X > 0

      N_Positive : Natural := 0;
      --  Number of positive values passed to Count
   end Genpos;

   package body Genpos is
      procedure Count (X : Num_T) is
      begin
         if X > 0 then
            N_Positive := N_Positive + 1;
         end if;
      end Count;
   end Genpos;

The body of ``Count`` features a decision.  Now consider the simple test
driver below:

.. code-block:: ada

   procedure Test_Genpos is
      type T1 is new Integer;
      package Pos_T1 is new Genpos (Num_T => T1);

      type T2 is new Integer;
      package Pos_T2 is new Genpos (Num_T => T2);
   begin
      Pos_T1.Count (X => 1);
      Assert (Pos_T1.N_Positive = 1);

      Pos_T2.Count (X => -1);
      Assert (Pos_T2.N_Positive = 0);
   end Test_Genpos;

This instanciates the generic unit twice, and each instance exercises one
outcome of the decision only. The two combined together do exercise the
decision boths ways, though, and this is what |gcp| reports::

  gnatcov coverage --level=stmt+decision --annotate=xcov+ ...

  -- genpos.adb.xcov:

  100% of 2 lines covered
  Coverage level: stmt+decision
   1 .: package body Genpos is
   2 .:    procedure Count (X : Num_T) is
   3 .:    begin
   4 +:       if X > 0 then
   5 +:          N_Positive := N_Positive + 1;
   6 .:       end if;
   7 .:    end Count;
   8 .: end Genpos;

Separated coverage (experimental)
---------------------------------

As described above, by default a single coverage analysis of any source
construct is performed, consolidating all code copies generated by this
construct. For subrpograms, this means consolidation over all inlined
copies. For generic units, consolidation over all instances.

A finer-grained analysis is possible, where distinct copies of the code coming
from a given source construct are identified according to some criterion, and
a separate coverage assessment is made for each of these copies. The violation
of a coverage obligation carries an additional indication of which code copy
the violation is reported for.

|gcv| supports a general mechanism for such ''separated'' analyses, which has
several different modes, detailed in the following subsections.


Separation by routine (:option:`-S routine`)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In this mode, two code regions coming from the same source construct
will undergo separate coverage analyses if they occur in different
symbols of the executable file.
When a given subprogram is inlined in two different calling routines,
each inlined copy thus undergoes a separate coverage assessment.  In the
absence of inlining, this will also ensure that different instances of
the same generic unit will have separated coverage analyses, since the
compiler generates different symbol names for different program units. On
the other hand, if two distinct instances of a generic subprogram are
inlined within a single calling routine, they will undergo a single
coverage analysis since they now occur in the same symbol.


Separation by instance (:option:`-S instance`)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In this mode, two code regions coming from the same source construct will
undergo separate coverage analyses if they come from different generic
instances. This ensures accurate per-instance coverage analysis even in the
presence of inlining.

.. _c_macros:

Processing of C macros
----------------------

For source coverage purposes, Source Coverage Obligations for C are produced
after the preprocessing of sources, with two consequences of note:

- Macro expansions leading to code with conditionals will trigger coverage
  violations, and multiple "calls" to the same macro just multiply these as
  they yield distinct expansions.

- The source locations output by |gcv| for coverage violations within macro
  expansions designate preprocessed tokens at the macro expansion site,
  typically on the line of the macro invocation but with column numbers
  unrelated to what is visible in the source on this line.

Consider this C code for example:

.. code-block:: c

     1	#define COND_INC(cond,x,y) \
     2	  do {                     \
     3	    if (cond)              \
     4	      (x)++;               \
     5	    else                   \
     6	      (y)++;               \
     7	  } while(0)
     8	
     9	int main ()
    10	{
    11	  volatile x = 0, y = 0;
    12	
    13	  COND_INC(x == 0, x, y);
    14	  COND_INC(x == 0, x, y);
    15	}


The two macro invocations actually expand as:

.. code-block:: c

    13    do { if (x == 0) (x)++; else (y)++; } while(0);
    14    do { if (x == 0) (x)++; else (y)++; } while(0);


The expanded version is the basis of SCO identification process, so we have
one decision and two conditioned statements on line 13, likewise on line 14.

Only one of each is exercised at execution time, and a :option:`stmt+decision`
analysis on this program yields::

  2.1. STMT COVERAGE
  ------------------

  t.c:13:32: statement not executed
  t.c:14:20: statement not executed

  2 violations.

  2.2. DECISION COVERAGE
  ----------------------

  t.c:13:12: decision outcome FALSE never exercised
  t.c:14:12: decision outcome TRUE never exercised

  2 violations.

We do see one statement and one decision coverage violation per invocation,
different in the two cases since the ``x == 0`` test is True on the first
call and False on the second one. We also observe column numbers unrelated to
what the original source lines contain on line 13 and 14.

.. _optimization:

Optimization and non-coverable items
====================================

|gcp| essentially operates by relating execution traces to source entities of
interest thanks to debug information mapping machine code addresses to source
locations.  With optimization enabled, there sometimes is no machine code
attached to a given statement, for example when the statement is determined to
be redundant or when the machine code for it can be factorized with the
machine code for another statement. When the coverage status of a code-less
statement cannot be be inferred from that of other statements around, |gcp|
categorizes the statement as :dfn:`non-coverable`.

By default, nothing is said about non-coverable statements in the
:option:`=report` outputs and the corresponding lines are marked with a '.' in
annnotated sources, as for any other line to which no machine code is
attached.  Below is an example source annotated for statement coverage, where
absence of code for a couple of Ada statments was triggered by constant
propagation and inlining. The local ``Pos`` function is called only once, with
a constant argument such that only one alternative of the ``if`` statement is
taken. With :option:`-O1 -gnatn`, the compiler sees that the ``else`` part can
never be entered and no code is emitted at all for this alternative:

.. code-block:: ada

   4 .: procedure Test_Pos1 is
   5 .:    function Pos (X : Integer) return Boolean;
   6 .:    pragma Inline (Pos);
   7 .:
   8 .:    function Pos (X : Integer) return Boolean is
   9 .:    begin
  10 +:       if X > 0 then
  11 +:          Put_Line ("X is positive");
  12 +:          return True;
  13 .:       else
  14 .:          Put_Line ("X is not positive");
  15 .:          return False;
  16 .:       end if;
  17 .:    end Pos;
  18 .: begin
  19 +:    Assert (Pos (1) = True);
  20 .: end Test_Pos1;

A common similar case is that of debugging code inhibited on purpose for
regular operation, for example with constructs like::

  Debug_Mode : constant Boolean := False;    or     #define DEBUG_MODE 0
  ...                                               ...
  if Debug_Mode then                                #if DEBUG_MODE

Another way to get this in Ada is with generic instanciations where constant
parameters turn what appears to be conditional in the source into a constant
value in some instances.

Back to our ``Test_Pos1`` example, no code is emitted for the test on line
10 either. |gcv| is however able to infer the ``if`` coverage status by
looking at the status of statements controlled by the decision, and the
Decision coverage report remains accurate:

.. code-block:: ada

    8 .:    function Pos (X : Integer) return Boolean is
    9 .:    begin
   10 !:       if X > 0 then
 decision "X > 0" at 10:10 outcome FALSE never exercised
   11 +:          Put_Line ("X is positive");
   12 +:          return True;

|gcvcov| features the :option:`--non-coverable` command line option to expose
the non-coverable statements if needed. They are listed in an additional
"``NON COVERABLE ITEMS``" section of the :option:`=report` outputs and the
corresponding lines are flagged with a '0' mark in annotated sources, as well
as a specific color in the html formats. For our example, this yields:

.. code-block:: ada

  10 !:       if X > 0 then
  11 +:          Put_Line ("X is positive");
  12 +:          return True;
  13 .:       else
  14 0:          Put_Line ("X is not positive");
  15 0:          return False;
  16 .:       end if;

