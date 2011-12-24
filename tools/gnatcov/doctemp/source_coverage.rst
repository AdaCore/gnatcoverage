************************
Source Coverage Analysis
************************

General principles & Compilation requirements
=============================================

Source coverage analysis computes metrics focused on source programming
language entities such as high level `statements` or `decisions` (DO178
parlance for boolean expressions). Machine object code is entirely
abstracted away.

For this purpose, |gcp| relies on :term:`Source Coverage Obligation` (SCO)
tables, compact descriptions of the nature and source location of program
entities relevant to source coverage criteria.  These tables are part of the
Library Information produced by the |gpro| compilers, in the .ali or .gli
file corresponding to each Ada or C unit, respectively.

The generation of SCO tables is triggered by the :option:`-gnateS` compilation
option for Ada, and by the :option:`-fdump-scos` option for C. These options
must be used to compile the sources you wish to analyze later on.

In addition, all the sources must also be compiled :option:`-g`
:option:`-fpreserve-control-flow`, both necessary to allow an accurate mapping
of the execution traces back to source level obligations, and optimization is
supported up to :option:`-O1`, with inlining allowed.
 
Once your application is built, the general process to perform source coverage
analysis proceeds in two steps: use |gcvrun| to produce execution traces, then
|gcvcov| to generate coverage reports.

The :ref:`gnatcov_run-commandline` section of this document provides details on
the trace production interface. The remainder of this chapter explains the use
of |gcvcov| in particular, to analyse traces once they have been produced.

.. _gnatcov_src_coverage-commandline:

``gnatcov`` ``(source)`` ``coverage`` command line
==================================================

Source coverage analysis is performed by invoking |gcvcov| for a source level
criterion via the :option:`--level` command line option.

The general interface synopsis is available from ``gnatcov`` ``--help``,
as follows:

::

 coverage OPTIONS TRACE_FILES
   Generate coverage report
   -c LEVEL --level=LEVEL     Specify coverage levels
      LEVEL is one of branch|insn|stmt|stmt+decision|stmt+mcdc|stmt+uc_mcdc
   -a FORM  --annotate=FORM    Generate a FORM report
      FORM is one of asm,xcov,html,xcov+,html+,report
   --routines=<ROUTINE|@FILE>  Add ROUTINE, or all routine listed
                               in FILE to the list of routines
   --scos=<FILE|@LISTFILE>     Consider all the SCOs in ALI file
                               FILE for this operation; or do that
                               for each file listed in LISTFILE
   --output-dir=DIR            Put the =html|xcov outputs into DIR
   -o FILE                     Put the =report output into FILE
   -T|--trace <FILE|@LISTFILE> Add FILE or all the files listed in
                               LISTFILE to the list of traces

:option:`-c`, :option:`--level` |marg| :
   Request the assessment of a specific set of coverage criteria.  The
   possible values for source level analysis are ``stmt``, ``stmt+decision``
   and variants of ``stmt+mcdc``, described in detail in later sections of
   this documentation.

:option:`-a`, :option:`--annotate` |marg| :
   Request a specific output report format.  The relevant values for source
   level analysis are ``xcov[+]``, ``html[+]`` and ``report``, all described
   in the :ref:`sreport-formats` section.

:option:`--routines`:
   This is specific to object coverage analysis and is described in the
   :ref:`gnatcov_obj_coverage-commandline` section of this documentation.

:option:`--output-dir` :
   Request that the report files (index and annotated sources for the ``xcov``
   and ``html`` output formats) be output in the provided directory. They are
   output in the current directory, where |gcv|, is launched, otherwise.
 
:option:`-o` :
   Request that the synthetic report produced by ``--annotate=report`` be
   output in the provided filname, instead of standard output by default.

:option:`--scos` |marg|, |rarg| :
   Provide the set of source units for which the requested coverage level is
   to be assessed, by the way of the corresponding Library Information files
   containing the relevant SCOs. Each instance of this option on the command
   line accumulates the provided set to what is to be assessed eventually.

:option:`-T`, :option:`--trace` |marg|, |rarg| :
   Provide the set of execution traces for which a report is to be
   produced. When multiple traces are provided, |gcv| produces a consolidated
   result, as if there had been a single execution producing one trace that
   would have been the catenation of all the individual traces.  See the
   :ref:`consolidation` section for a description of the consolidation
   facility.

Elements on the command line that are not tied to a particular option are
considered as trace file arguments. :option:`--trace` is marked mandatory only
to indicate that at least one trace file is required, which may but need not
be introduced with :option:`-T` or :option:`--trace`.

Here are a few examples of valid command lines:

::

  gnatcov coverage --level=stmt --annotate=report --trace=myprog.trace
  # statement coverage assessment for a single trace,
  # synthetic text report on standard output

  gnatcov coverage --level=stmt+decision --annotate=html prog1.trace prog2.trace
  # statement and decision coverage assessment for two traces stated as two
  # instances of orphan arguments. html report files in current directory

  gnatcov coverage --level=stmt+decision --annotate=html @mytraces
  # Same report, with prog1.trace and prog2.trace listed in the
  # "mytraces" text file

The following sections now describe the available report formats, then
provide more details and examples regarding the supported coverage criteria.

.. _sreport-formats:

Output report formats
=====================

Source coverage reports may be produced in various formats, as requested
with the :option:`--annotate` option to |gcvcov|.

Annotated sources, html : :option:`--annotate=html[+]`
------------------------------------------------------

:option:`--annotate=html` produces essentially

- One `.html` browsable annotated source file per compilation unit for which
  source coverage obligations were provided,

- An `index.html` page which summarizes the assessment context (assessed
  criteria, set of trace files involved, with their tags, ...) and the
  coverage results for all the units, with links to their annotated
  sources.

Each annotated source page features a header followed by the original source
lines, all numbered.

Prio to it's sequence number and the source text that follow, every line has a
single character indicative of a coverage status for the line. We call this
character a :dfn:`coverage annotation`, which may be one of the following:

.. csv-table::
   :delim: 10, 80
   :widths: 10, 80
   :header: Character, Meaning

   ``.`` 

In addition, each source line is colorized to reflect its associated coverage
completeness, with green, orange and red for ``+``, ``!`` or ``-`` coverage
respectively.

With the `+` extension, the annotated machine code for each line
may be expanded below it by a mouse click on the line.

Annotated sources, text : :option:`--annotate=xcov[+]`
------------------------------------------------------

Violations summary, text : :option:`--annotate=report`
------------------------------------------------------

:option:`--annotate=report` produces a syntetic text report of all the
coverage violations relevant to the set of criteria to be assessed per the
:option:`--level` argument.

General structure and example
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The synthetic reports features explicit start/end of report notifications and
four sections in between: Assessment Context, Non Exempted Violations,
Exempted Regions and Analysis Summary.  The general structure is sketched
below and a more detailed description of each report section follows.

::

  ** COVERAGE REPORT **

  ===========================
  == 1. ASSESSMENT CONTEXT ==
  ===========================
  ...
  =========================================
  == 2. NON-EXEMPTED COVERAGE VIOLATIONS ==
  =========================================
  ...
  =========================
  == 3. EXEMPTED REGIONS ==
  =========================
  ...
  =========================
  == 4. ANALYSIS SUMMARY ==
  =========================
  ...
  ** END OF REPORT **
  

Assessment Context
^^^^^^^^^^^^^^^^^^

The *Assessment Context* section exposes the following information items:

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

Non-exempted Coverage Violations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The *Non-exempted violations* section lists and counts the coverage violations
with that relate to source lines not part of an exemption region.  The
violations are grouped in subsections, one per assessed criterion according to
the :option:`--level` option:

.. csv-table::
   :delim: |
   :header: :option:`--level=`, Assessed criteria / Report subsections
   :widths: 10, 50

   `stmt`          | Statement Coverage
   `stmt+decision` | Statement and Decision Coverage
   `stmt+mcdc`     | Statement, Decision and MCDC Coverage


All the non-exempted violations are reported using a consistent
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


Here is an example output excerpt for :option:`--level=stmt+mcdc`, with
one subsection for each of the three criteria requested at that level:

::

  =========================================
  == 2. NON-EXEMPTED COVERAGE VIOLATIONS ==
  =========================================

  2.1. STMT COVERAGE
  ------------------

  engines.adb:71:10: statement not executed

  1 violation.

  2.2. DECISION COVERAGE
  ----------------------

  engines.adb:70:14: decision outcome TRUE never exercised

  1 violation.

  2.3. MCDC COVERAGE
  ------------------

  engines.adb:34:14: condition has no independent influence pair, MC/DC not achieved
  engines.adb:46:13: condition has no independent influence pair, MC/DC not achieved

  2 violations.


When multiple violations apply someplace, the most salliant diagnostic is
emitted alone. For instance, if an Ada statement like "`X := A and then B;`"
is not covered at all, a "`statement not executed`" violation is emitted
alone, even if we're assessing for, say, `--level=stmt+decision` ; |gcv| emits
no decision oriented violation in this case.

Exempted Regions
^^^^^^^^^^^^^^^^

The *Exempted violations* section lists and counts the exempted regions,
displaying for each the source location span, the number of actually exempted
violations in the region, and the exemption justification text. For example:

::

  =========================
  == 3. EXEMPTED REGIONS ==
  =========================

  assert.adb:22:4-27:4: 2 exempted violations, justification:
  assertions are expected never to fail

  1 exempted region.

See the :ref:`exemptions` section 
  
Analysis Summary
^^^^^^^^^^^^^^^^

The *Analysis Summary* section summarizes just the counts reported in each of
the previous report sections.  For our example report so far, this would be:

::

  =========================
  == 4. ANALYSIS SUMMARY ==
  =========================

  1 non-exempted STMT violation.
  1 non-exempted DECISION violation.
  2 non-exempted MCDC violations.
  1 exempted region.

  
This section provides a quick way to determine whether the requested coverage
level, as conveyed by :option:`--level`, is fully satisfied, with details available
from the per criterion sections that precede.


Statement Coverage (SC) assessments
===================================

Statement coverage is achieved with :option:`--level=stmt`, together with
:option:`--scos` to provide the set of SCOs of interest via ALI files.
The `xcov` and `html` annotation formats both generate a
representation of the sources with annotations on each relevant line,
according to the following table:

@multitable @columnfractions .1 .8
* @h:term:`Note` @tab @h:term:`Means ...`
* '`.`'
@tab no SCO or no executable code for this line
* '`-`'
@tab statement uncovered (not executed) on this line
* '`+`'
@tab statement covered (executed) on this line
@end multitable

Below is a sample session to illustrate on the Explore example, for the
`robots` unit after recompilation with *-gnateS -O0*.  Note the *--level*
option passed to both `run` and `coverage` invocations::

  $ gnatcov run --level=stmt explore
  ... run session, trace goes to explore.trace by default ...

  $ gnatcov coverage --level=stmt --scos=obj/robots.ali --annotate=xcov explore.trace  

To analyze a full set of units at once, just fetch the list of ALI files in a
list and provide an `}file to @code{--scos`.  For instance, in a Unix-like
environment::

  $ ls obj/*.ali > alis
  $ gnatcov coverage --scos=@alis --level=stmt --annotate=xcov explore.trace

  
.. highlight:: ada

For the `Stations` unit, this produces a `stations.adb.xcov`
output with::

  Coverage level: STMT
  87% of 38 lines covered
  [...]
    74 .:       function Control_For (C : Character) return Robot_Control;
    75 .:       --  Map user input character C to Robot_Control command, Nop if
    76 .:       --  the input isn't recognized.
    77 .:
    78 .:       function Control_For
    79 .:         (C : Character) return Robot_Control is
    80 .:       begin
    81 +:          case C is
    82 .:             when 'p' | 'P' =>
    83 +:                return (Code => Probe, Value => 0);
    84 .:             when 's' | 'S' =>
    85 +:                return (Code => Step_Forward, Value => 0);
    86 .:             when 'l' | 'L' =>
    87 -:                return (Code => Rotate_Left, Value => 0);
    88 .:             when 'r' | 'R' =>
    89 -:                return (Code => Rotate_Right, Value => 0);

`--annotate=report` instead simply diagnoses the set of source
lines with uncovered statements, for example like:


::

  stations.adb:87: statement not executed
  stations.adb:89: statement not executed

  

More details on the report format are available in a dedicated
appendix of this documentation.
By default, the report goes to standard output.
It may be directed to a file instead, with the addition of a
*-o <filename>* option on the command line.

Decision Coverage (DC) assessments
==================================

|gcv| features combined Statement and Decision Coverage assessment
capabilities with :option:`--level=stmt+decision`.

We consider to be :dfn:`decisions` all the boolean expressions used
to influence the control flow via explicit constructs in the source
program, such as ``if`` statements or ``while`` loops.

For proper operation, expressions may only resort to short-circuit operators
to combine operands.  The |gnat| compilers offer the
`No_Direct_Boolean_Operator` restriction pragma to make sure this rule is
obeyed.

A decision is said fully covered when tests were made so that the
decision has evaluated to both true and false.

If only one of these two possible outcomes was exercised, the decision
is said partially covered.
The case where none of the possible decision outcomes was exercised
happens when the enclosing statement was not executed at all, or when
all the attempted evaluations were interrupted e.g. because of
exceptions.
Uncovered statements remain reported as such, without further details
even if there are decisions therein.

The `xcov` and `html` annotation formats both generate a
representation of the sources with annotations at the beginning of
each relevant line, according to the following table:

@multitable @columnfractions .1 .8
* @h:term:`Note` @tab @h:term:`Means ...`
* '`.`'
@tab no SCO or no executable code for this line
* '`-`'
@tab statement uncovered on this line
* '`!`'
@tab decision partially covered on this line
* '`+`'
@tab all the decisions on this line are fully covered
@end multitable

As for object coverage, additional information is available on request
with an extra `+` suffix on the annotation format, that is, with
`--annotate=xcov+` or `html+`.
Extra details are typically provided for decisions partially covered,
with information about which outcome was not exercised.

The `--annotate=report` synthetic output lists information about
uncovered statements and partial decision coverage.
For example, after exercising Explore to have the robot execute safe
commands in both Cautious and Dumb modes, we get the expected results
below on a sample of the `Robots` control code:


::

    $ gnatcov coverage --level=stmt+decision --annotate=report
      --scos=obj/powerpc-elf/robots.ali explore.trace
    ...
    robots.adb:56:9: decision outcome TRUE never exercised
    robots.adb:75:10: decision outcome TRUE never exercised
    robots.adb:78: statement not executed

  

For decision related diagnostics, the source location features both a
line and a column number to designate the first token of the decision
unambiguously.
Below is the corresponding `--annotate=xcov+` output excerpt.
Decision diagnostics are always expanded on the first line of the
decision:


::

    [...]
    51 .:    function Unsafe (Cmd : Robot_Command; Sqa : Square) ...
    52 .:    begin
    53 .:       --  Stepping forward with a block or a water pit ahead is Unsafe
    54 .:
    55 +:       return
    56 !:         Cmd = Step_Forward
  DECISION "Cmd = Ste..." at 56:9: outcome TRUE never exercised
    57 !:         and then (Sqa = Block or else Sqa = Water);
    58 .:    end Unsafe;
    [...]
    64 .:    procedure Process_Next_Control
    65 .:      (Port : Robot_Control_Links.IOport_Access)
    66 .:    is
    [...]
    73 .:       --  Cautious, the robot refuses to process unsafe controls
    74 .:
    75 !:       if Robot.Mode = Cautious
  DECISION "Robot.Mod..." at 75:10: outcome TRUE never exercised
    76 !:         and then Unsafe (Ctrl.Code, Probe_Ahead (Robot.Hw.Rad))
    77 .:       then
    78 -:          return;
    79 .:       end if;
    [...]

  

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

.. _exemptions:

Coverage Exemptions
===================

In some circumstances, there are good and well understood reasons why proper
coverage of some source statement or decision is not achievable, and it is
convenient to be able to abstract these coverage violations away from the
genuine defects of a testing campaign.  The |gcp| :dfn:`exemptions` facility
was designed for this purpose.

For Ada with the |gnat| compilers, coverage exemptions are requested for
sections of source by the insertion of dedicated pragmas:

- ``pragma Annotate (Xcov, Exempt_On, "justification text");`` starts a
  section, providing some justification text that will be recalled in coverage
  reports.

- ``pragma Annotate (Xcov, Exempt_Off);`` closes the current exemption section.

There may be no overlap between exemption regions.

Exempted regions are reported as blocks in both the annotated source
and the synthetic text reports.

In annotated source reports, a ``#`` or ``*`` caracter annotates all the
exempted lines, depending on whether 0 or at least 1 violation was exempted
over the whole section, respectively.

In synthetic text reports, a single indication is emitted for each exempted
region, and the indications for all the regions are grouped in a separate
report section.



