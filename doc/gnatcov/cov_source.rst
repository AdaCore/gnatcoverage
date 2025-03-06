.. _scov:

**************************************
Source coverage analysis with |gcvcov|
**************************************

.. _scov-principles:

Source coverage analysis computes metrics focused on source programming
language entities such as high level *statements* or *decisions* (|do|
parlance for boolean expressions), which translate as :term:`Source Coverage
Obligations <Source Coverage Obligation>`, or SCOs, in |gcp| terms.

Once you have produced coverage traces, actual analysis is performed
with |gcvcov| to generate coverage reports. *Source* coverage is
queried by passing a specific :cmd-option:`--level` argument to
|gcvcov|.

The general structure of this command line is always like::

  gnatcov coverage --level=<criterion> --annotate=<format>
                   <units-of-interest> ... <traces-or-checkpoints>

The next sections describe the :ref:`available report formats
<sreport-formats>`, then provide more details regarding :ref:`scov-stmt`,
:ref:`scov-dc`, and :ref:`scov-mcdc`.

The :ref:`sunits` chapter describes how application units to be considered for
coverage assessment are to be specified.

|gcvcov| command line
=====================

.. index::
   single: gnatcov coverage command line for source coverage

.. _gnatcov_coverage-commandline-src:

Coverage analysis with |gcp| is performed by invoking |gcvcov| for a set of
critera queried via the :cmd-option:`--level` command line option. The general
interface synopsis is available from |gcv| :cmd-option:`--help`::

 gnatcov coverage OPTIONS TRACE_FILES

The available options are as follows:

:cmd-option:`-c`, :cmd-option:`--level` |marg|:
   Tell the set of coverage criteria to be assessed. The possible values for
   source coverage analysis are :cmd-option:`stmt`,
   :cmd-option:`stmt+decision`, :cmd-option:`stmt+mcdc`, and
   :cmd-option:`stmt+uc_mcdc`, all explained later in this chapter.

:cmd-option:`-a`, :cmd-option:`--annotate` |marg|:
   Request one or more output report formats. All the criteria support
   ``cobertura``, ``html,``, ``report``, ``xcov[+]``, ``xml`` formats, with
   interpretations that vary depending on the assessed criteria. See the
   corresponding documentation later in this chapter for more details. This
   option accepts comma separated values and/or can be specified multiple times
   on the command line, in which case there will be one report produced for each
   of the requested formats.

:cmd-option:`-o` :
   Request that the synthetic report produced by ``--annotate=report`` be
   output in the provided filname instead of standard output by default. This
   is just ignored for other output formats.

.. include:: cov_common_switches.rst

:cmd-option:`-t`, :cmd-option:`--target` :
   .. include:: target_switch_common_text.rst

:cmd-option:`--non-coverable`:
   Report about language statements for which no object code could be found in
   the surrounding suprogram (typically out of optimization).

:cmd-option:`-P`:
   Use the indicated project file as the root project to select the units of
   interest for the analysis and find default options. Default options are
   taken only from this project. In absence of :cmd-option:`--projects` and of
   :cmd-option:`--no-subprojects`, the units of interest are those designated
   by this project and all it's transitive dependencies, minus those advertised
   as externally-built.  The target/runtime/scenario contextual options that
   would be passed to build the project should also be passed for proper
   interpretation of the project files.  See the :ref:`sunits` chapter of this
   manual for more details.

:cmd-option:`--projects`, |rarg|:
   When using :cmd-option:`-P`, use the provided projects to select units of
   interest, together with their transitive dependencies unless
   :cmd-option:`--no-subprojects` is also provided. The projects designated by
   this option must all be part of the import transitive closure reachable from
   the root project designated by :cmd-option:`-P`.

:cmd-option:`--no-subprojects`:
  Consider only the projects (and units) encompassed by the :cmd-option:`-P` /
  :cmd-option:`--projects` options as of-interest. Don't include any project
  imported from there.

:cmd-option:`--units`, |rarg|:
   When using project files, override the list of units of interest for
   source coverage with those provided.

:cmd-option:`--subdirs`:
   When using project files, look for :term:`Library Information files <Library
   Information file>` in the indicated subdirectory of each project's object
   directory.

:cmd-option:`--scos`, |rarg|:
   Provide the set of
   :term:`Library Information files <Library Information file>` from which
   Source Coverage Obligations (SCOs) should be loaded to process **binary**
   traces. This low-level switch effectively overrides the project based units
   of interest selection.  It only has effect if used with binary traces.

:cmd-option:`--sid`, |rarg|:
   Provide the set of :term:`Source Instrumentation Data files <Source
   Instrumentation Data file>` from which Source Coverage Obligations (SCOs)
   should be loaded to process **source** traces. This low level switch
   effectively overrides the project based units of interest selection. It only
   has effect if used with source traces.

:cmd-option:`--ignore-source-files`, |rarg|:
   Provide a list of globbing patterns (as in Unix shells) of source
   files to be excluded from the analysis and from the output report.
   See the :ref:`ignore_source_files` section for more information.

:cmd-option:`--dump-units-to`:
   For source coverage analysis specifically, output the names of units that
   are considered of-interest to the requested assessment, that is, for which a
   report or checkpoint is going to be produced. This also outputs, for each
   unit of interest, the list of files that were individually ignored using the
   ``Ignored_Source_Files`` project attribute or corresponding command-line
   option. Ignored source files listed with :cmd-option:`--dump-units-to` will
   be either marked as ``always ignored``, if they were ignored in all the
   inputs (traces or checkpoints) that were used to produce this report, or as
   ``sometimes ignored``, if the source files were ignored in at least one of
   the inputs of this report, but not all of them. The argument may be either
   the name of a file, clobbered it if it already exists, or '-' to request
   displaying the list on standard output. In the latter case, when a
   ``report`` output is also requested, the list of units is displayed as an
   additional report section.

:cmd-option:`--save-checkpoint`:
    Save the resulting coverage analysis to the named checkpoint file.

:cmd-option:`--checkpoint`, |rarg|:
    Load previously saved coverage analysis checkpoint(s), and continue coverage
    analysis from that initial state.

:cmd-option:`--source-rebase`, |rarg|:
    Specify alternate absolute path prefixes to locate source files when
    producing annotated source outputs. See :ref:`rebase_opts`

:cmd-option:`--source-search`, |rarg|:
    Specify a directory in which missing source files will be searched when
    producing annotated source outputs. See :ref:`rebase_opts`

A lot of options are available to control the set of units for which coverage
is to be assessed. They may be combined in multiple ways and attributed within
the project files are available to refine the set of units to include or
exclude from each designated project. See :ref:`sunits` for extra details, and
:ref:`using-gpr` for a general overview of how the project facilities operate.

Saving coverage analysis state checkpoints allows the production of
consolidated results from successive runs of the ``coverage`` command.
In particular this allows coverage results to be computed incrementally,
and allows consolidation with different sets of units of interest,
in order to avoid incidental coverage. See :ref:`checkpoints` for a
discussion of these use cases.

Positional arguments on the command line (not tied to a particular option) are
considered as trace file arguments. At least one trace file is required for the
``coverage`` command to operate, which may but need not be introduced with
:cmd-option:`-T` or :cmd-option:`--trace`. Here are a few examples of valid
command lines to illustrate. Other examples will be exposed along the course of
the following sections::

  gnatcov coverage --level=stmt -P ut.gpr --annotate=report --trace=prog.srctrace
  #                      (a)       (b)            (c)             (d)
  # (a) Request Statement coverage assessment,
  # (b) for units belonging to the "ut.gpr" project,
  # (c) producing a synthetic text report on standard output (no -o option),
  # (d) out of a single execution trace "prog.trace".

  gnatcov coverage --level=stmt+decision -P ut.gpr --annotate=html t1 t2
  # Statement and Decision coverage assessments for two traces "t1" and "t2",
  # producing an html report in the current directory.

  gnatcov coverage --level=stmt+decision -P ut.gpr --annotate=html @mytraces
  # Same report, with t1 and t2 listed in the "mytraces" text file

  gnatcov coverage --level=stmt -Papp.gpr --annotate=html @mytraces
  # Same kind of report, for Statement coverage only, on source units owned
  # by "app.gpr" and its transitive closure of project dependencies.

  gnatcov coverage --level=stmt -Papp.gpr --no-subprojects --annotate=html @mytraces
  # Likewise, considering only the units owned by app.gpr



.. _sreport-formats:

Output report formats (:cmd-option:`--annotate`)
================================================

Source coverage reports may be produced in various formats, as requested with
the :cmd-option:`--annotate` command line argument of |gcvcov|. The
:cmd-option:`xcov` and :cmd-option:`html` formats produce a set of annotated
source files, in the directory where |gcv| is launched unless overriden with a
:cmd-option:`--output-dir` option. The :cmd-option:`report` output consists in a
synthetic text report of :term:`coverage violations <Coverage Violation>` with
respect to the requested criteria, produced on standard output by default or in
the file specified by the :cmd-option:`-o` command line option. The
:cmd-option:`cobertura` produce a single xml file containing a coverage report
in the Cobertura format. Its name is by default `cobertura.xml` and can be
altered using the :cmd-option:`-o` command line option.

Later in this chapter we name output formats by the text to add to
:cmd-option:`--annotate` on the command line. For example, we use "the
:cmd-option:`=report` outputs" to mean "the coverage reports produced with
:cmd-option:`--annotate=report`".

We will illustrate the various formats with samples extracted from outputs
obtained by perfoming coverage analysis of the following example Ada
application unit:

.. code-block:: ada

   function Between (X1, X2, V : Integer) return Boolean;
   --  Is V between X1 and X2, inclusive and regardless of their ordering?

   function Between (X1, X2, V : Integer) return Boolean is
   begin
      if X1 < X2 then
         return V >= X1 and then V <= X2;
      else
         return V >= X2 and then V <= X1;
      end if;
   end Between;


Cobertura report (:cmd-option:`=cobertura`)
-------------------------------------------

:cmd-option:`--annotate=cobertura` produces a coverage report in the Cobertura
format, as specified per the cobertura.dtd (document type description) that is
generated alongside.

This format specification is not maintained by gnatcov, and it does not thus
provide all of the information that other report formats do, notably MC/DC
coverage information, violation messages. Decision violations are output using
the "branch" terminology of Cobertura (one decision being two branches, and
either 0, 1 or 2 of those branches are covered).

It is mainly provided for integration with external tools, such as continuous
integration systems, e.g. gitlab, which supports integration of coverage reports
into merge requests using this format.

File names in this coverage report are absolute. Depending on the use context of
this coverage report, it can also make sense to strip a given prefix from the
absolute paths to make them relative to, e.g. a project root. The
:cmd-option:`--source-root` command line option accepts a string prefix that
will be removed from absolute path references in the report.


Annotated sources, html (:cmd-option:`=html`)
---------------------------------------------

For source coverage criteria, |gcvcov| :cmd-option:`--annotate=html` produces an
index-based report under the HTML format (the other command names are aliases
for backward compatibility).

To navigate the report, open the index.html file using the browser of your
choice. This index file contains a summary of the assessment context (assessed
criteria, trace files involved, ...) and of the coverage results for all the
units, with links to their annotated sources. If the :cmd-option:`-P` was used
to designate the source units of interest, sources are indexed per-project.

Note that some dynamic filtering / sorting can be applied:

* Filter by kind of coverage obligations: either reporting on lines, or on
  statement / decision / MCDC (one or several) obligations, depending on the
  coverage level. See :ref:`synthetic-metrics` for more information.

* Sort indexes by clicking on column headers, allowing for example sorts keyed
  on unit names or on relative coverage achievement.

See our :ref:`sample html index <sample_sc_html_index>` appendix for an
example index page, which embeds a self-description of all the items it
contains.

The user can browse through an annotated version of the sources from the index.
Each annotated source page contains a summary of the assessment results. This
summary can be expanded to print subprogram metrics: the user can click on a
subprogram's metrics to access it in the annotated source immediately. This
summary is followed by the original source lines, all numbered and marked with a
coverage annotation as in the :cmd-option:`--annotate=xcov` case. Lines with
obligations are colorized in green, orange or red for ``+``, ``!`` or ``-``
coverage respectively.

See the :ref:`sample annotated source <sample_sc_html_unit>` appendix for a
sample of html annotated source.


SARIF report (:cmd-option:`=sarif`)
-----------------------------------

For source coverage criteria, |gcvcov| :cmd-option:`--annotate=sarif` produces
a JSON-based report under the Static Analysis Results Interchange Format
(SARIF).

The generation of SARIF reports is mainly provided to allow viewing gnatcov
coverage reports with external IDEs that supports viewing them (for example,
Visual Studio Code with Microsoft's SARIF Viewer extension). To navigate this
report, open the ``coverage.sarif`` file using an integrated development
environment which supports viewing SARIF results.

The reports contain information on the violations observed by gnatcov as
"Results" and the enabled coverage levels as "Rules". Each violation is
registered as "error". Exempted violations are reported as "note" and
undetermined coverage results as "warning".

Violations summary, text (:cmd-option:`=report`)
------------------------------------------------

For source coverage criteria, |gcvcov| :cmd-option:`--annotate=report` produces
a summary that lists all the :term:`coverage violations <Coverage Violation>`
(failure to satisfy some aspect of a coverage criterion) relevant to the set of
assessed criteria.

The report features explicit start/end of report notifications and at least
three sections in between: Assessment Context, Coverage Violations, and
Analysis Summary. Should |gcv| be unable to determine the coverage state of
some coverage obligations, those will be reported in a dedicated Undetermined
Coverage Items section, with a description of why the tool was unable to
determine the coverage state for each obligation. A few variations are
introduced when :term:`exemption regions <Exemption Region>` are in scope.
See the :ref:`exemptions` section for more details on their use and effect on
the output reports.

If :cmd-option:`--dump-units-to -` is also on the command line, a *UNITS OF
INTEREST* section is produced, which contains the list of units considered
of-interest for the reported assessment, as well as the list of source files
individually ignored with the ``Ignored_Source_Files`` project attribute and
corresponding command-line option.


Annotated sources, text (:cmd-option:`=xcov[+]`)
------------------------------------------------

For source coverage criteria, |gcvcov| :cmd-option:`--annotate=xcov` produces
an annotated version of each source file, in text format, named after the
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
   ``?`` | Coverage obligations attached to the line, undetermined coverage state (*)
   ``+`` | Coverage obligations attached to the line, all satisfied

(*) The Undetermined Coverage state (``?``) is only shown on the line in the
absence of other known violations for that same line.

Here is, to illustrate, the full statement coverage report produced for our
example unit when the ``Between`` function was called so that the ``if``
control evaluated True only. The function is actually part of an Ada package,
called Ranges, with an original body source file named ``ranges.adb``:

.. code-block::

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

:cmd-option:`--annotate=xcov+` (with a trailing +) works the same, only
providing extra details below lines with improperly satisfied obligations. The
available details consists in the list of :term:`coverage violations <Coverage
Violation>` diagnosed for the line, which depends on the coverage criteria
involved. Here is an excerpt for our previous example, where the only
improperly satisfied obligation is an uncovered statement on line 7::

   7 -:          return V >= X2 and then V <= X1;
   STATEMENT "return V ..." at 7:10 not executed


XML report, xml (:cmd-option:`=xml`)
------------------------------------

:cmd-option:`--annotate=xml` produces a coverage report in the XML format, as
specified per the ``gnatcov-xml-report.xsd`` (XML schema description) that is
generated alongside.

This report format is on par with the HTML report in terms of features, and it
is the preferred choice for programmatically accessing the coverage results.


Assessment Context
^^^^^^^^^^^^^^^^^^

The *Assessment Context* report section exposes the following information
items:

* Date & time when the report was produced

* Command line and Version of |gcp| that produced the report. The set of units
  that the report is about is conveyed by the command line switches summarized
  there (:cmd-option:`--projects`, :cmd-option:`--units`,
  :cmd-option:`--scos`).

* Coverage level requested to be analyzed

* Details on the input trace files, such as the path to the binary
  program exercised (as recorded in the trace header) or the trace
  production time stamp.

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
the :cmd-option:`--level` option:

.. tabularcolumns:: ll
.. csv-table::
   :delim: |
   :header: :cmd-option:`--level=`, Assessed criteria / Report subsections
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
assessing :cmd-option:`--level=stmt+mcdc` and we also have improper decision
and conditions coverage.

Here is an output excerpt for our example with :cmd-option:`--level=stmt+mcdc`,
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

Statement Coverage analysis
===========================

Core notions and Reporting (:cmd-option:`--level=stmt`)
-------------------------------------------------------

|gcv| performs Statement Coverage assessments with the
:cmd-option:`--level=stmt` command line option. The assessment determines the
status of statement coverage obligations out of the tests execution,
considering that:

* A statement is :dfn:`covered`, and the obligation :dfn:`discharged`,
  as soon as the control flow has reached the statement.

* A statement is :dfn:`uncovered` otherwise.

There is no notion of partial coverage for statements, even when a
statement execution is interrupted somehow, for example by an
exception occurrence. For instance, the statement below will be
reported as covered as soon as the statement is reached, even if the
expression evaluation never really terminates::

  X := Function_That_Raises_Exception (Y) + Z;

In synthetic :cmd-option:`=report` outputs, unexecuted source statements are
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
   ``!`` | More than one statement on the line, some covered
   ``?`` | At least one statement on this line, some with undetermined coverage state (*)
   ``+`` | At least one statement on the line, all covered

(*) The Undetermined Coverage state (``?``) is only shown on the line in the
absence of other known violations for that same line.

When a single statement spans multiple lines, the coverage annotation is
present on all the lines, as the two ``+`` signs for the single assignment
in the following excerpt::

  3 .:  -- A single assignment spanning two lines:
  4 +:  Result :=
  5 +:     Input1 * Input2;

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

Note that if no executable code for a given unit can be found in any of the
executables submitted to gnatcov, then all statements in the unit will
be conservatively reported as uncovered. This ensures that if tests
for an entire unit have been omitted from a test campaign, a violation
will be properly reported. Such violations can be suppressed either using
exemptions, or by removing the unit from the list of units of interest.

Example program and assessments
-------------------------------

Let us consider the example functional unit below, with the spec and
body stored in source files named ``div_with_check.ads`` and
``div_with_check.adb``:

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

We first exercise the function for Y = 1 only, using the following :term:`test
driver <Test Driver>` in ``test_div1.adb``:

.. code-block:: ada

   procedure Test_Div1  is
      X : constant Integer := 4;
   begin
      Assert (Div_With_Check (X, 1) = X);
   end;

From a ``test_div1.srctrace`` obtained by building the instrumented source
produced with |gcvins|, we analyze for the Statement Coverage criterion using
the following |gcvcov| invocation::

  gnatcov coverage --level=stmt -P ut.gpr --annotate=xcov test_div1.srctrace

We get an :cmd-option:`=xcov` annotated source result in text format for the
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

The :cmd-option:`=xcov` outputs follow. First, for the functional unit, with
the ``if`` statement coverage reversed compared to the previous testcase::

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

The corresponding synthetic report is simply obtained by running |gcvcov| again
with :cmd-option:`--annotate=report` instead of :cmd-option:`--annotate=xcov`::

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

We can see here that the two lines marked ``-`` in the :cmd-option:`=xcov`
outputs are properly reported as violations in the ``STMT COVERAGE`` section of
this report, and that this section is the only one presented in the ``COVERAGE
VIOLATIONS`` part, as only this criterion was to be analyzed per the
:cmd-option:`--level=stmt` argument.

.. _scov-dc:

Decision Coverage analysis
==========================

Core notions and Reporting (:cmd-option:`--level=stmt+decision`)
----------------------------------------------------------------

With the :cmd-option:`--level=stmt+decision` command line option, |gcv|
performs Statement and Decision Coverage assessments combined together.
:dfn:`Decisions` in this context are defined as:

* Any Boolean expression used to influence the control flow via
  explicit constructs in the source program, such as ``if`` statements
  or ``while`` loops;

* The controlling predicate of Ada if-expressions or C
  conditional-expressions, in any kind of context except Ada
  contracts.

* The iteration predicate of Ada quantified expressions, in the same
  contexts as for if-expressions.

* The iterator filter predicate optionally present in a loop parameter
  specification or in an iterator specification, in the same context as
  if-expressions.

The expression may be of essentially any type in C. In Ada, this may
be the standard Boolean type, or subtypes of it, or derived types
thereof. Here are a few examples show-casing decisions in a variety of
contexts::

  while (z++) ...  /* "z++", controlling a while-loop.  */

  if X > 0 then ...  -- "X > 0", controlling an if-statement

  q = z-- ? x : y;  /* "z++", conditional-expression predicate.  */

  Q := (X if Z < 2 else Y); -- "Z < 2", if-expression predicate

  Z := (for all X of Container => P(X));  -- "P(X)", quantified-expression predicate
  T := (for some X of Container => P(X));

  for I in 1 .. 10 when (I mod 2) = 0 loop ... -- "(I mod 2) = 0", iterator filter predicate
  F := (for Elt of Container when Pred (Elt) => Elt); -- "Pred (Elt)", iterator filter predicate


The coverage status of a decision obligation is determined as
follows:

* A decision is said :dfn:`fully covered`, or just :dfn:`covered`, and
  the obligation discharged, as soon as the decision has been evaluated at
  least once True and once False during program execution.

* A decision is said :dfn:`uncovered` when none of the possible
  outcomes was exercised, either because the enclosing statement was
  not executed at all or when all the attempted evaluations were
  interrupted e.g. because of exceptions.

* If only one of the two possible outcomes was exercised, the
  decision is said :dfn:`partially covered`, and the obligation
  only partially discharged.

When a decision is part of a statement and the statement is not executed at
all, only the statement level violation is reported. The nested decision level
violations are implicit in this case and diagnosing them as well would only
add redundancy.

The :cmd-option:`=report` synthetic output lists the statement and decision
coverage violations in the ``STMT`` and ``DECISION`` coverage report section
respectively.  For the :cmd-option:`=xcov` and :cmd-option:`=html`
annotated-source oriented formats, the single annotation produced on each
source line combines the statement and decision coverage indications. The
following table summarizes the meaning of the possible annotations:


.. tabularcolumns:: cl
.. csv-table::
  :delim: |
  :widths: 10, 80
  :header: Annotation, Meaning

   ``-`` | At least one statement on the line, none executed.
   ``!`` | Unless multiple statements are involved, decision partially covered on the line.
   ``?`` | At least one statement or decision on the line with undetermined coverage state. (*)
   ``+`` | All the statements and decisions on the line are covered.

(*) The Undetermined Coverage state (``?``) is only shown on the line in the
absence of other known violations for that same line.

When a trailing `+` is added to the format passed to :cmd-option:`--annotate`
(:cmd-option:`=xcov+`), a precise description of the actual violations is
available for each line in addition to the annotation. The :cmd-option:`=html`
provides it by default.

Example program and assessments
-------------------------------

Consider the example functional Ada unit below, with the spec and body
stored in source files named ``divmod.ads`` and ``divmod.adb``:

.. code-block:: ada

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
True. As a result, the only :cmd-option:`stmt+decision` violation by our driver
is the ``Tell`` decision coverage, only partially achieved since we have only
exercised the True case. This is confirmed by :cmd-option:`=report` excerpt
below, where we find the two violations sections in accordance with the
requested set of criteria::

   2.1. STMT COVERAGE
   ------------------

   No violation.

   2.2. DECISION COVERAGE
   ----------------------

   divmod.adb:14:10: decision outcome FALSE never exercised

   1 violation.

For :cmd-option:`--annotate=xcov`, this translates as a single partial coverage
annotation on the inner ``if`` control line::

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

This results in the following :cmd-option:`=xcov` output::

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

This gets all confirmed by the :cmd-option:`=report` output below, on which we
also notice that the only diagnostic emitted for the uncovered inner ``if`` on
line 14 is the statement coverage violation::

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

Modified Condition/Decision Coverage analysis
=============================================

Core notions and Reporting  (:cmd-option:`--level=stmt+mcdc`)
-------------------------------------------------------------

Combined Statement and Modified Condition/Decision Coverage (MCDC) analysis is
performed by passing the :cmd-option:`--level=stmt+mcdc` option to |gcvcov|
commands. :dfn:`Decisions` in this context are defined as:

* All the expressions considered as decisions for decision coverage,
  as well as:

* All the Boolean expressions which combine operands with
  *short-circuit* logical operators ("``&&``" and "``||``" in C,
  "``and then``" and "``or else``" by default in Ada), in any kind of
  context except Ada contracts.

Then for all the decisions in the sources of interest:

* Testing shall exercise both the True and False outcomes of the expression as
  a whole and demonstrate the :dfn:`independant influence` of :dfn:`conditions`
  in |do| parlance, where ...

* Separate conditions in a decision are identified as the operands of
  *short-circuit* operators.

Regarding coverage status definitions:

* A condition is :dfn:`covered`, and the obligation discharged, when
  the independant effect on the enclosing decision was demonstrated by
  the tests,

* A condition is said :dfn:`uncovered` otherwise.

The :ref:`mcdc-decisions` section that follows provides a few examples to
illustrate the identification of decisions and conditions.
:ref:`non-short-circuit` focuses on the handling of computational Boolean
operators, then :ref:`mcdc-variants` expands on the notion of
:dfn:`independant influence` and on possible variations of the MCDC criterion
definition.

Output-wise, the source annotations for the :cmd-option:`=xcov` or
:cmd-option:`=html` formats are the same as for decision coverage, with
condition specific cases marked with a ``!`` as well:

.. tabularcolumns:: cl
.. csv-table::
   :delim: |
   :widths: 10, 60
   :header: Annotation, Meaning

   ``-`` | At least one statement associated with this line, none executed.
   ``!`` | For a single statement line, decision partially covered or condition not covered on the line.
   ``?`` | At least one statment or decision on the line with undetermined coverage state. (*)
   ``+`` | All the statements, decisions and conditions on the line are covered.

(*) The Undetermined Coverage state (``?``) is only shown on the line in the
absence of other known violations for that same line.

The :cmd-option:`=report` outputs feature an extra MCDC section in the Coverage
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

.. _mcdc-decisions:

Example decisions and conditions
--------------------------------

As described in the previous section, the set of expressions considered as
*decisions* for MCDC is wider than for decision coverage analysis, which
encompasses all the expressions used as conditionals in explicit control-flow
construct, for example:

.. code-block:: ada

  if Check(X) then -- 1 decision (if-stmt control)

.. code-block:: c

  while (x > 12) { // 1 decision (while loop control)

MCDC also includes the conditionals of Ada *if-expressions*
or they C *conditional-expression* counterpart, as in:

.. code-block:: ada

  Val := (if X > 0 then X else -X); -- 1 decision (if-expr control)

.. code-block:: c

  return (x > y) ? 1 : -1; // 1 decision (cond-expr control)

Then we have logical expressions combining operands with short-circuit
operators in other contexts, such as:

.. code-block:: ada

  X := A and then B; -- 1 decision (short-circuit and-then)
  Check (C or else D); -- 1 decision (short-circuit or-else)

.. code-block:: c

  return (x < low || y > high); // 1 decision (short-circuit ||)

This can raise lead to various cases of multiple decisions within
a single expression, for example:

.. code-block:: ada

    if Check (Arg1 => A and then B, Arg2 => C or else D) then
    -- 3 decisions here:
    -- * A and then B (short circuit and-then)
    -- * C or else D (short circuit or-else)
    -- * Check (...)  (if-stmt control)

In C as in Ada, logical negation is allowed anywhere and influences
neither decision boundaries nor the split into conditions:

.. code-block:: c

   if (x && !y) { // 1 decision (if-stmt control)
                  // 2 conditions (short-circuit &&)

.. code-block:: ada

   if not (A or else B) then -- 1 decision (if-stmt control)
                             -- 2 conditions (short-circuit or-else)

.. _non-short-circuit:

Handling non short-circuit operators
------------------------------------

Short-circuit operators are key to instating a decision at all in some
contexts, and to direct the split of decisions into conditions in all
contexts. By default, non short-circuit binary operators are taken as regular
computational devices:

.. code-block:: ada

  if A or B then -- 1 decision (if-statement control)
                 -- single condition (no short-circuit by default)

  if A or else B then -- 1 decision (if-statement control)
                      -- 2 conditions (short-circuit or-else)

  X := A and B;  -- no decision (no short-circuit by default)

  X := A and then B;  -- 1 decision (short-circuit and-then)
                      -- 2 conditions (short-circuit and-then)

This computational view for non short-circuit operators in Ada
is similar to the C model where bitwise operations on non purely
Boolean types, yet used as a Boolean result, are extremely common
as in:

.. code-block:: c

    return (x & 0x3) && (y & 0x3);  // 1 decision (short-circuit &&)
                                    // 2 conditions, each a & on int

It can also result in splits introducing the possibility of mulitple
decisions in arguments, as in:

.. code-block:: ada

  if ((A and then not B) == (C or else (D and then E))) then
  -- 3 decisions here:
  -- * The toplevel expression as a whole (... == ...), if-stmt control,
  --   single condition.
  -- * (A and then not B), short-circuit and-then in the first operand of ==,
  --   2 conditions.
  -- * (C or else (D and then E)), short-circuit tree in the second operand,
  --   3 conditions.

|gnat| compilers offer two devices to mitigate possible issues caused
by this behavior on non short-circuit Boolean operators for Ada:

* To make sure operators of this kind are not used, e.g. as a coding standard
  rule, a ``No_Direct_Boolean_Operator`` restriction pragma can be setup in
  the development environment to trigger compilation errors on such uses;
  Alternatively:

* The ``Short_Circuit_And_Or`` configuration pragma requests that the
  non-short circuit ``and`` and ``or`` operators on standard Booleans are
  compiled as as their short-circuit counterparts, then processed as such for
  coverage analysis.

There is no equivalent in C, where the allowed operand types are much more
varied and where the restriction would make the language really much harder to
use.

.. _mcdc-variants:

MCDC variants
-------------

Compared to Decision Coverage, achieving MCDC requires tests that demonstrate
the independent influence of conditions in decisions. Several variants of the
criterion exist.

:dfn:`Unique Cause MCDC` is the original criterion described in the |do|
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


Each line in such a table is called an :term:`evaluation vector <Evaluation
Vector>`, and the pairs that demonstrate the independant effect of conditions
are known as :term:`independence pairs <Independence Pair>`.

Evaluations 1 + 3 constitute a Unique Cause independence pair for A, where A
changes, B does not, and the expression value toggles. 1 + 2 constitues a pair
for B.

The closest criterion supported by |gcp| is a very minor variation where
conditions that are not evaluated due to short-circuit semantics are allowed
to differ as well in a pair. Indeed, their value change cannot possibly have
influenced the decision toggle (since they are not even considered in the
computation), so they can never invalidate the effect of another condition.

We call this variation :dfn:`Unique Cause + Short-Circuit MCDC`, activated with
:cmd-option:`--level=stmt+uc_mcdc` on the command line. From the ``A and then
B`` table just introduced, 4 + 1 becomes another valid independence pair for A,
as `B` is not evaluated at all when `A` is False so the change on `B` is
irrelevant in the decision switch.

:cmd-option:`--level=stmt+mcdc` actually implements another variant, known as
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

After instrumentation for MCDC and execution, we can then request,
say, an :cmd-option:`=xcov+` report to get a first set of results in
the ``ranges.adb.xcov`` annotated source::

   gnatcov coverage --level=stmt+mcdc -Pmytest.gpr --annotate=xcov+ test_x1vx2.srctrace

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
the :cmd-option:`=report` output::


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

.. _scov-atc:

Assertion True Coverage (ATC) analysis (experimental)
=====================================================

Along with the previous coverage levels, |gcv| can provide different strictness
levels of assertion coverage for source traces for Ada.

Assertion coverage warrants defining separate criteria to perform coverage
analysis since contrary to all other regular decisions, those contained in
assertions are expected to never be evaluated to False. Assertion decisions and
conditions are defined the same way as for Decision and MCDC but, the coverage
is computed differently.

If such coverage analysis is needed, it should always be activated along one of
the previously described coverage levels, as an "addition". In the following
sections on assertion coverage, their associated command line options will be
written as `--level=...+<assertion coverage level>` where `...` is one of
`stmt`, `stmt+decision`, `stmt+mcdc` and `stmt+uc_mcdc`.

Using assertion coverage levels allows to compute the coverage of the pragma
statements `Assert`, `Assert_And_Cut`, `Assume`, `Check`, `Loop_Invariant`, and
the aspects `Type_Invariant`, `Precondition` and `Postcondition`.

Core notions and Reporting (:cmd-option:`--level=...+atc`)
----------------------------------------------------------

|gcv| performs Assertion True Coverage assessments with the
:cmd-option:`--level=...+atc` command line option. The assessment determines
the status of assertion true coverage obligations out of the tests execution,
considering that:

* An assertion is :dfn:`covered`, and the obligation :dfn:`discharged`,
  as soon as the assertion's decision has been evaluated once to True.

* An assertion is :dfn:`uncovered` otherwise.

In synthetic :cmd-option:`=report` outputs, uncovered source assertions are
listed as ATC Coverage violations in the report section dedicated to these.

In annotated source outputs, the coverage annotations convey the following
indications:

.. tabularcolumns:: cl
.. csv-table::
   :delim: |
   :widths: 10, 80
   :header: Annotation, Meaning

   ``-`` | At least one assertion on the line, none covered
   ``!`` | More than one assertion on the line, some covered
   ``?`` | At least one assertion on this line, some with undetermined coverage state (*)
   ``+`` | At least one assertion on the line, all covered

(*) The Undetermined Coverage state (``?``) is only shown on the line in the
absence of other known violations for that same line.

.. _scov-atcc:

Assertion True Condition Coverage (ATCC) analysis (experimental)
================================================================

Core notions and Reporting (:cmd-option:`--level=...+atcc`)
-----------------------------------------------------------

The coverage status of an "ATCC" obligation is determined as follows:

* An assertion is said :dfn:`fully covered`, or just :dfn:`covered`, and the
  obligation discharged, as soon as all conditions have been evaluated to True
  or False at least once accross all evaluations to True of the whole decision.

* An assertion is said :dfn:`uncovered` when the decision was never
  evaluated to True, either because the enclosing assertion statement was not
  executed at all or when all the attempted evaluations were interrupted e.g.
  because of exceptions.

* An assertion is said :dfn:`partially covered` when at least one of the
  conditions of the assertion's decision was never evaluated to either True or
  False in the context of an evaluation to True of the whole decision. In this
  case, the obligation is partially discharged.

The :cmd-option:`=report` synthetic output lists the ATC and ATCC coverage
violations in the ``ATC`` and ``ATCC`` coverage report section respectively.
For the :cmd-option:`=xcov` and :cmd-option:`=html` annotated-source oriented
formats, the single annotation produced on each source line combines the
statement and ATC coverage indications. The following table summarizes the
meaning of the possible annotations:

.. tabularcolumns:: cl
.. csv-table::
  :delim: |
  :widths: 10, 80
  :header: Annotation, Meaning

   ``-`` | At least one statement on the line, none executed.
   ``!`` | Unless multiple statements are involved, assertion partially covered on the line.
   ``?`` | At least one statement or assertion on the line with undetermined coverage state. (*)
   ``+`` | All the statements and assertions on the line are covered.

(*) The Undetermined Coverage state (``?``) is only shown on the line in the
absence of other known violations for that same line.

When a trailing `+` is added to the format passed to :cmd-option:`--annotate`
(:cmd-option:`=xcov+`), a precise description of the actual violations is
available for each line in addition to the annotation. The :cmd-option:`=html`
provides it by default.

.. _scov-fun_call:

Function and Call Coverage (FUN_CALL) analysis (experimental)
=============================================================

|gcv| can also provide an analysis of the coverage of subprograms and calls for
Ada, C, and C++ source traces.

If such coverage analysis is needed, it should always be activated along one of
the non-assertion coverage levels previously described. In this section on
function and call coverage, the associated command line option will be written
as `--level=...+fun_call` where `...` is one of `stmt`, `stmt+decision`,
`stmt+mcdc` and `stmt+uc_mcdc`. Assertion coverage can also be activated at the
same time.

Core notions and Reporting (:cmd-option:`--level=...+fun_call`)
---------------------------------------------------------------

|gcv| performs Function and Call Coverage assessments with the
:cmd-option:`--level=...+fun_call` command line option. The assessment
determines the status of subprograms and call coverage obligations out of the
tests execution, considering that:

* A subprogram is :dfn:`covered`, and the obligation :dfn:`discharged`,
  as soon as the execution as entered it at least once.

* A subprogram is :dfn:`uncovered` otherwise.

* A call is :dfn:`covered`, and the obligation :dfn:`discharged`, if it has
  been executed at least once.

* A call is :dfn:`uncovered` otherwise.

In synthetic :cmd-option:`=report` outputs, uncovered source subprograms and
calls are listed as FUN_CALL Coverage violations in the dedicated report
section.

It is important to note that for an :dfn:`uncovered` statement which happens
to be a call statement, only a statement violation will be emitted in
coverage reports.

The limitations of this coverage level are detailed in the
:ref:`instr-limitations` section.

.. _scov-gexpr:

Guarded Expression Coverage (GEXPR) analysis (experimental)
============================================================

For the Ada language, |gcv| can provide an analysis on coverage of guarded
expressions.

Core notions and Reporting (:cmd-option:`--level=...+gexpr`)
------------------------------------------------------------

|gcv| the following syntaxes to be :dfn:`guarded expressions`:

* Then and Else dependent expressions of :dfn:`if expressions`

.. code-block:: ada

   procedure Foo (A : Boolean) is
      Var : constant String :=
        (if A
         then "True expression"
         else "False expression");
   begin
      null;
   end Foo;

* Dependent expressions of :dfn:`case expressions`

.. code-block:: ada

   type Animal is (Cat, Dog, Cow);

   procedure Foo (A : Animal) is
      Var : constant String :=
        (case A
         when Cat    => "Expression Cat",
         when Dog    => "Expression Dog",
         when others => "Expression other");
   begin
      null;
   end Foo;

* Predicate expression of :dfn:`quantified expressions`

.. code-block:: ada

   function Prime_In_Range (L, R : Natural) return Boolean is
   begin

      --  Is_Prime (I) is the child expression that will be analysized.

      return (for some I in (L .. R) => Is_Prime (I));
   end Foo;

For each of these, we consider the expression to be :dfn:`covered` and the
obligation to be :dfn:`discharged` if the execution flow evaluated it at least
once.

The limitations of this coverage level are detailed in the
:ref:`instr-limitations` section.

.. _synthetic-metrics:

Synthetic metrics
=================

A number of output formats feature :dfn:`synthetic metrics`
(essentially, coverage percentages) for each source unit, and for the
entire analysis when an index summary is also produced. We first
introduce the metrics definition for the ``=xcov`` format then
describe how they are presented in other outputs.

For the ``=xcov`` format, the synthetic metrics are summary numbers
at the top the annotated source for each unit. For example::

  .../metrics/main.adb:
  50% of 4 lines covered
  75% statement coverage (3 out of 4)
  0% decision coverage (0 out of 1)

  Coverage level: stmt+decision
   1 .: with Ada.Text_IO; use Ada.Text_IO;
   2 .: with Types; use Types;
   3 .:
   4 .: procedure main is
   5 +:    X : My_Int := 12 with Volatile;
   6 .: begin
   7 !:    if X < 2 then
   8 -:       Put_Line ("X LT 2");
   9 .:    else
  10 +:       Put_Line ("X GE 2");
  11 .:    end if;
  12 .: end;


The metric displayed first is::

  50% of 4 lines covered

This is *line count* based, where the count includes all the lines to which is
attached at least one coverage obligation of the asserted level (here,
``stmt+decision``). In the provided example, these are lines 5, 7, 8 and 10,
for a total of 4 lines, where line 7 encompasses two obligations (one
statement and one decision).

The metric conveys the total number of lines and the percentage
of such lines for which all the obligations are fully covered, that is,
those marked with a '+' annotation.

The two other metrics in this example are::

  75% statement coverage (3 out of 4)
  0% decision coverage (0 out of 1)

These are *coverage obligation count* based, one for each obligation
kind involved in the assessed level (here, for
``--level=stmt+decision``). As for lines, each metric displays the
total number of obligations of the given kind and the ratio of such
obligations which have been fully discharged. The ``xcov`` obligation
metrics don't distinguish partially covered from uncovered items. This
information is available from the ``html`` report.

For the ``html`` output, synthetic metrics for all the units are
first displayed on the index page, together with metrics for the
entire analysis and for sets of units grouped by GPR project.

Initially, the line count based metrics are displayed, as illustrated
by :numref:`html-index-lines`. The main procedure source used in our ``xcov``
report example is the ``main.adb`` source here, single source encompassed
by the ``main.gpr`` project:

Switching to coverage obligation count based metrics is simply achieved by
clicking the button(s) at the top of the page labelled with obligation kind
names ("Stmt", "Decision" or "Mcdc").

Each kind of obligation can be selected alone. Selecting multiple
kinds is also allowed and just sums the individual counts.
:numref:`html-index-obligations` illustrates the results we get with
the ``Stmt`` and ``Decision`` kinds selected together for our previous
example:

.. _html-index-lines:

.. figure:: html-index-lines.*
  :scale: 42%
  :align: center

  Html index with line count based synthetic metrics

.. _html-index-obligations:

.. figure:: html-index-obligations.*
  :scale: 42%
  :align: center

  Html index with obligations count based synthetic metrics (stmt+decision)


.. _rebase_opts:

Handling source relocation for annotated sources output formats
===============================================================

For all annotated sources output formats
(:cmd-option:`--annotate=xcov[+]|html)`, |gcv| needs access to the sources of
the :term:`units of interest <Units of Interest>` to generate the output. The
tool searches for sources in some default locations which are usually correct
when the whole coverage analysis process is done on the same machine.

When coverage reports are produced on a different machine than the one
where the coverage run was done, or when sources are moved in the
interim, |gcv| will likely not be able to automatically find the
source files. The tool offers two options to help in those situations:

* The option :cmd-option:`--source-rebase=\<old_prefix\>=\<new_prefix\>`

  This option allows the specification of alternate path prefixes for source
  files.

  If the location of a source file, when built on a first machine, was
  ``/work/build_1234/src/main.adb`` but on the machine where the report is to
  be generated, the same file is located at
  ``/some/path/project/src/main.adb``, then in order for |gcv| to find this
  file, the option
  :cmd-option:`--source-rebase=/work/build_1234/=/some/path/project/` can be
  passed. Note that the prefixes apply to absolute paths.

  This option supports globbing expressions for the ``old_prefix`` part of the
  option, so in the example above, if the build number varies from run to run,
  the option passed can be
  :cmd-option:`--source-rebase=/work/build_*=/some/path/project/`.

  This option also allows for response-files
  (:cmd-option:`--source-rebase=\@rebase_file`) in which each line must be of
  the form ``old_prefix=new_prefix``.

  This option can be passed multiple times on the command line, and |gcv| will
  try each pair in the order in which they were passed until the source file
  is found.

* The option :cmd-option:`--source-search=directory_path`

  This option allows the specification of directories in which to search for
  missing source files.

  If the location of a source file, when built on a first machine,
  was ``/work/build_1234/src/main.adb`` but its location on the machine where
  the report is generated is ``/some/path/project/src/main.adb`` then passing
  :cmd-option:`--source-search=/some/path/project/src` will enable |gcv| to
  find the missing source file. This option also accepts response files.

  This option can appear multiple times on the command line. Note that |gcv|
  will first try the :cmd-option:`--source-rebase` prefix pairs, if any, and
  if the source file was not found, it will then look for  the file in all
  the directories passed with :cmd-option:`--source-search` in the order in
  which they appear on the command line.

These two options perform very similar functions and can sometimes be used
interchangeably, however the second option is less selective and can lead to
unexpected results in some circumstances. Since :cmd-option:`--source-search`
only specifies an additional directory in which to search for missing sources,
it does not guarantee that if a file is found then it is the correct one, but
only that the file found has the same basename than the searched file. For Ada
projects this should not be an issue, since all files should have different
names, but for C projects where there can be homonyms, this can be problematic:

If a project is built with the files ``/work/build_1234/src1/foo.h`` and
``/work/build_1234/src2/foo.h`` among its sources, and
:cmd-option:`--source-search=/some/path/src1 --source-search=/some/path/src2`
is passed to |gcv| to add the two source directories to the list of locations
where sources can be found, then the resulting location for both ``foo.h``
files will be ``/some/path/src1/foo.h``. In this case, passing
:cmd-option:`--source-rebase=/work/build=/some/path/` removes the ambiguity.

:cmd-option:`--source-rebase` is also more convenient when the project has
multiple source directories, provided that their structure is the same when
building/instrumenting the project and generating the coverage report. In the
example above, it is necessary to pass :cmd-option:`--source-search` twice to
include all the source directories in the search path, but only one instance of
:cmd-option:`--source-rebase` is needed. For very large projects, since the
number of required instances of :cmd-option:`--source-search` is equal to the
number of source directories, it is clearly more advantageous to use
:cmd-option:`--source-rebase` which only needs to be passed once.

Inlining & Ada generic units
============================

In the vast majority of situations, inlining is just transparent to source
coverage metrics: calls are treated as regular statements, and coverage of the
inlined bodies is reported on the corresponding sources regardless of their
actual inlining status. See the :ref:`optimization` section for a description
of effects that might show up on rare occasions.

Generic units have a different status, where each instantiation produces a
distinct, potentially unique instance of the generic code working on a
different types, objects, with different helper subprograms or packages.

|gcp| offers two main kinds of coverage strategies for generic units: one
where the generic source is considered as the entity of interest, to which all
the instances contribute, and one where each instance is considered as a
separate entity of interest.

.. _generics_cov:

Combined coverage on generics
-----------------------------

Ada generic units are also uniformly treated as single source entities, with
the coverage achieved by all the instances combined and reported against the
generic source only, not for each individual instance.

Consider the following functional Ada generic unit for example. It provides a
simple vector type abstraction on which two operations are available; ``Inc``
adds some amount to each element of a vector, and ``Mult`` multiplies each
element by some amount. The exposed type is of fixed size, provided as a
parameter:

.. code-block:: ada

   generic                               -- vops.ads
      Size : in Integer;
   package Vops is
      type Vector_Type is array (1 .. Size) of Integer;

      procedure Inc (V : in out Vector_Type; Amount : Integer);
      procedure Mult (V : in out Vector_Type; Amount : Integer);
   end;

   package body Vops is                  -- vops.adb

      procedure Inc (V : in out Vector_Type; Amount : Integer) is
      begin
         for I in V'Range loop
            V(I) := V(I) + Amount;
         end loop;
      end;

      procedure Mult (V : in out Vector_Type; Amount : Integer) is
      begin
         for I in V'Range loop
            V(I) := V(I) * Amount;
         end loop;
      end;
   end;

Now consider this test, checking operations on vectors of different
sizes, from two instances of the ``Vops`` unit:

.. code-block:: ada

   with Vops;                            -- v5.ads
   package V5 is new Vops (Size => 5);

   with Vops;                            -- v8.ads
   package V8 is new Vops (Size => 8);

   with V5, V8;                          -- test_5inc_8mult.adb
   procedure Test_5inc_8mult is
      V5o : V5.Vector_Type := (others => 1);
      V8o : V8.Vector_Type := (others => 2);
   begin
      V5.Inc (V5o, 3);
      V8.Mult (V8o, 2);
   end;

Only the ``Inc`` subprogram is called through the V5 instance and only
the ``Mult`` subprogram is called through the V8 instance. Both suprograms
are nevertheless called overall, so the ``Vops`` package body is claimed
fully covered by default::

 gnatcov coverage -Pvops.gpr --level=stmt --annotate=xcov test_5inc_8mult.trace
 ...
 100% of 4 lines covered
 Coverage level: stmt
   1 .: package body Vops is
   2 .:
   3 .:    procedure Inc (V : in out Vector_Type; Amount : Integer) is
   4 .:    begin
   5 +:       for I in V'Range loop
   6 +:          V(I) := V(I) + Amount;
   7 .:       end loop;
   8 .:    end;
   9 .:
  10 .:    procedure Mult (V : in out Vector_Type; Amount : Integer) is
  11 .:    begin
  12 +:       for I in V'Range loop
  13 +:          V(I) := V(I) * Amount;
  14 .:       end loop;
  15 .:    end;
  16 .: end;


Handling Ada assertions and contracts
=====================================

Ada 2012 introduced sophisticated *programming by contract* mechanisms which
generalize the base ``pragma Assert`` construct originally offered by the
language. All these facilities allow asserting various properties of the
program state at specific points of its execution, for example as type
invariants or as Pre/Post-conditions on subprograms.

Each property is expressed a Boolean expression expected to hold True,
triggering an exception otherwise. When the current assertion policy activates
a given assertion construct, the associated Boolean expression is treated by
|gcv| as a decision for MCDC purposes.

As assertions are by construction designed never to evaluate False, reaching
proper coverage for them is non-trivial, if not entirely meaningless, for
post-conditions in particular.

The simple way out consists in disabling the relevant constructs in builds
intended for coverage analysis, by setting the corresponding
``Assertion_Policy`` to ``Disable`` with GNAT Pro toolchains.

.. _c_macros:

Processing of C macros
======================

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
   :linenos:

   #define COND_INC(cond,x,y) \
     do {                     \
       if (cond)              \
         (x)++;               \
       else                   \
         (y)++;               \
     } while(0)

   int main ()
   {
     volatile x = 0, y = 0;

     COND_INC(x == 0, x, y);
     COND_INC(x == 0, x, y);
   }


The two macro invocations actually expand as:

.. code-block:: c
   :linenos:
   :lineno-start: 13

       do { if (x == 0) (x)++; else (y)++; } while(0);
       do { if (x == 0) (x)++; else (y)++; } while(0);


The expanded version is the basis of SCO identification process, so we have one
decision and two conditioned statements on line 13, likewise on line 14. Only
one of each is exercised at execution time, and a :cmd-option:`stmt+decision`
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
:cmd-option:`=report` outputs and the corresponding lines are marked with a '.'
in annnotated sources, as for any other line to which no machine code is
attached.  Below is an example source annotated for statement coverage, where
absence of code for a couple of Ada statments was triggered by constant
propagation and inlining. The local ``Pos`` function is called only once, with
a constant argument such that only one alternative of the ``if`` statement is
taken. With :cmd-option:`-O1 -gnatn`, the compiler sees that the ``else`` part
can never be entered and no code is emitted at all for this alternative::

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
Decision coverage report remains accurate::

    8 .:    function Pos (X : Integer) return Boolean is
    9 .:    begin
   10 !:       if X > 0 then
 decision "X > 0" at 10:10 outcome FALSE never exercised
   11 +:          Put_Line ("X is positive");
   12 +:          return True;

|gcvcov| features the :cmd-option:`--non-coverable` command line option to
expose the non-coverable statements if needed. They are listed in an additional
"``NON COVERABLE ITEMS``" section of the :cmd-option:`=report` outputs and the
corresponding lines are flagged with a '0' mark in annotated sources, as well
as a specific color in the html formats. For our example, this yields::

  10 !:       if X > 0 then
  11 +:          Put_Line ("X is positive");
  12 +:          return True;
  13 .:       else
  14 0:          Put_Line ("X is not positive");
  15 0:          return False;
  16 .:       end if;
