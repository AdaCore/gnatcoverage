***************
Getting Started
***************

General principles and common notions
=====================================

|gcp| provides a range of coverage analysis facilities with support for a
variety of coverage criteria, several output formats and consolidation
features to produce aggregated reports out of partial results.

Actual coverage is always first computed out of *trace files* conveying what
test programs have achieved. |gcp| works with two possible kinds of traces:

- :dfn:`Binary traces`, produced by an instrumented execution environement
  while running an unmodifed version of the program. Such traces contain low
  level information about executed blocks of machine instructions.

- :dfn:`Source traces`, produced by an alternative version of the program,
  built from sources instrumented to feed coverage dedicated datastructures.

Both kinds of traces can be used to assess so called :dfn:`source coverage`
criteria, where the entities subject to coverage assessment are defined in
terms of source level constructs. The specific criteria that |gcp| supports
are those defined by the DO-178B certification standard for civil avionics:

- :dfn:`Statement Coverage`, where the coverage metrics are respective to source
  level statements such as a variable assignment or a subprogram calls;

- :dfn:`Decision Coverage`, which, in addition to statement coverage, requires
  evaluating Boolean expressions (*decisions* in DO178B parlance) both True
  and False, then

- :dfn:`Modified Condition/Decision Coverage`, commonly known as *MCDC*, which
  requires testing particular variations of individual Boolean operands
  (*conditions* in DO178B parlance) within decisions.


A central notion to all the assessments performed with |gcp| is that of
:dfn:`units of interest`, which simply designates the set of compilation units
of which we are aiming to assess the coverage. These typically include the
code under test in a campaign, as opposed to, for example, the sources of the
test harness infrastructure when one is used. The individual statements,
decisions, or conditions of relevance within units of interest are referred to
as :dfn:`Source Coverage Obligations` or :dfn:`SCOs`.

From binary traces, |gcp| is also able to produce *object coverage* reports,
measuring the coverage of machine level instructions produced by the
compilation toolchain out of the original sources. |gcp| supports two criteria
of this kind:

- :dfn:`Instruction Coverage`, where we evaluate for each machine instruction
  whether it has been executed at least once or not; and

- :dfn:`Branch Coverage`, where, in addition, we evaluate for each conditional
  branch instruction whether it was only taken, or went fallthrough or both.


As the later parts of this manual with describe, the processes involved in
producing binary or source traces differ in some ways. We don't support mixing
the two methods together in a single coverage campaign, so a particular scheme
needs to be selected upfront for each project. We have however strived to
maximize the commonalities between the two solutions, in order to facilitate
transitions from one scheme to the other and ensure that the widest possible
range of improvements (for example, to output report formats) benefit both
schemes in a consistent manner.

.. _support:

Supported languages and environments
====================================

Object coverage analysis is essentially language agnostic and available for
both native Linux/Windows applications and for cross environments where we can
obtain binary traces. For such configurations, we would normally rely on
GNATemulator or on hardware probes to produce the traces.

Source coverage analysis, on the other hand, is by nature defined in
close association with each particular source language.

As of today, with binary traces, |gcp| supports all the variants of Ada
and C supported by the compilation toolchain, for native Linux or Windows
applications as well as for a number of cross configurations with
Zero-Footprint or Ravenscar runtimes for Ada.

With source traces, obtained from instrumented source programs, only Ada is
supported at this stage, in native or cross environments. A few limitations
remain compared to binary traces, in particular:

- The ``Short_Circuit_And_Or`` pragma is not handled by the source
  instrumenter: non-short-circuit ``and`` and ``or`` operators are always
  considered as intra-condition computational operators;

- Separate analysis of generic package instances is not supported.

On the other hand, source traces allow coverage analysis on code running from
shared libraries, which we don't support for binary traces.

With both kinds of traces, the behavior on Ada 2012 *case* expressions is
still subject to change, in particular regarding decision or MCDC analysis as
the criteria definition aren't yet well established for such constructs in
general.

Selecting binary or source traces
=================================

A given coverage assessment for a project may not mix source and binary traces
together. The choice of a trace format depends on a number of considerations.

The first aspect is the match between the project's needs and what each format
allows, as described in the :ref:`support` section. For object coverage
assessments, binary traces is the only option.  For source coverage
assessements, the matrix below summarizes the possibiliites:

.. list-table::
   :widths: 20 20 20 20
   :header-rows: 1
   :class: table-bordered

   * -
     - Ada language
     - C language
     - Shared Libraries
   * - Native
     - Source or Binary
     - Binary only
     - Source only
   * - Cross
     - Source or Binary
     - Binary only
     - N/A


When source or binary traces are both an option, the selection can be
performed according to a balance between test execution performance and the
setup conveniency/adquation with possible project organizational requirements.

On the one hand, binary traces are produced by execution monitors which incur
a very significant execution slow down in native environments, in addition to
requiring compilation options which restrict optimization in several
ways. Their main advantage, however, is they operate without explicit
additional data state or control flow deviations, since what gets tested for
coverage purposes is built from the same sources as what will run in
operational conditions.

Source traces, on the other hand, are produced out of an alternate version of
the program, with sources modified to include coverage related additional data
structures and code adjustments of multiple sorts to feed these structures at
run-time. The performance impact is expected to be much lower than with binary
traces, however, and the trace files are more compact as they convey much
higher level information.

Process overview
================

Integration within |gps| apart, the facilities offered by |gcp| are primarily
exposed through the |gcv| command-line tool and a coverage assessment process
always consists in the following high level steps:

- Arrange to produce traces (source or binary) from test programs, then

- Generate report(s) from the traces, either directly or via intermediate
  results latched in so called :term:`coverage checkpoint` files.

Some aspects of these operations depend on the targetted coverage criterion,
on the kind of trace involved or on the program target environment. A brief
overview is provided in the following sections of this chapter and further
details are available from more specific chapters of this manual.

A number of the following examples include a :option:`--level=`:option:`<>`
command line switch.  The intent is to convey a target coverage criterion when
needed, where ``<>`` would be ``stmt``, ``stmt+decision``, or ``stmt+mcdc``
for source coverage criteria; ``insn`` or ``branch`` for object coverage
criteria.

Quite a few example command lines also include as
:option:`<units-of-interest>` placeholder, which represents a set of switches
conveying the set of units for interest for source coverage
assessments. Project files provide the most elaborate mechanisms for this
purpose, with switches allowing the specification of *projects of interest*
starting from a root project file, and optional attributes in individual
project files, allowing a fine grained description of which particular units
are of interest there, if not all.  See the :ref:`sunits` section of this
manual for a detailed description of the available actual options.

Producing binary traces (``gnatcov run|convert``)
-------------------------------------------------

For binary traces, |gcv| relies on an instrumented execution environment to
produce the traces instead of having to instrument the program itself with
extra code and data structures soleley aimed at tracking facts of interest for
coverage purposes.

For cross configurations, |gem| provides such an environment, offering support
for coverage assessments directly on the target code. Hardware probes may also
be used as trace producers, provided trace data is converted to the format
|gcv| expects. Most Linux and Windows native configurations are supported as
well, using Valgrind or DynamoRIO, respectively, as process wrappers to
produce traces within the host environment.

We provide an outline of the steps involved here. More details are available
in the :ref:`bin_traces` separate chapter of this manual.

Programs are built from their original sources, only requiring specific
compilation options. At least :option:`-g -fpreserve-control-flow -fdump-scos`,
possibly others depending on the target configuration and needs you might have
for some amount of optimization.
Once a program is built, producing traces involves either:

- Using |gcvrun| to execute the program within an instrumented environment
  on the host, like::

     gnatcov run <yourapp> [--target=<target>] [--kernel=<kernel>]
       [--level=<>] [<units-of-interest>]  (implicit -o <yourapp.trace>)

  or, if you rely on on-board execution and have a hardware probe we support,

- Using |gcvcnv| to convert the trace produced by the probe, like::

    gnatcov convert --trace-source=<probe-id> --exec=<yourapp>
      --input=<probe-output> -o <yourapp.trace>

Very briefly here:

- :option:`--target` selects the execution environment that will know how to
  produce execution traces, such as <target>-gnatemu for emulated
  configurations.  This can also be achieved with a ``Target`` attribute in
  the project file designated by :option:`-P` (see the :ref:`target_attr`
  section of this manual).

  Absence of a target specification requests instrumented execution within the
  host environment, in which case command line arguments can be passed to the
  executable program, as described in the :ref:`execution-control` section of
  this manual.

- :option:`--kernel` is necessary for cross configurations where an operating
  system kernel such as VxWorks is needed to load and launch your applicative
  modules on top of the bare machine execution environment.

- :option:`--level` states the strictest coverage criterion which will be
  assessed from the resulting trace afterwards. To |gcvrun|, this is required
  for stmt+mcdc assessments only, in which case it is highly recommended, for
  efficiency reasons, to also state the :option:`<units-of-interest>` on which
  such analysis will (or at least might) be conducted.

Producing source traces (``gnatcov instrument``)
------------------------------------------------

The production of source traces is performed by an instrumented version of the
program running in its regular execution environment. An alternative version
of the program sources is generated for units of interest, with additional data
structures and code statements solely aimed at tracking coverage related
information. Coverage data is dumped out to source traces or some IO channel
according to a user selectable policy, for which the program main unit gets
instrumented as well.

We provide an outline of the steps involved here, illustrated for the case
of a native configuration. More details are available in the :ref:`src_traces`
separate chapter of this manual.

The whole scheme requires the use of project files. The instrumented code
relies on common data types and subprograms provided by a :term:`coverage
runtime` library, distributed in source form with |gcp|. The first thing to do
for a given project is then to build and install this coverage runtime so it
becomes available to the instrumented sources afterwards. The easiest way to
achieve this consists in first making a copy of the coverage runtime sources, then
build and install from there, as documented in the :ref:`instr-rts` section of
the aforementioned chapter.

Once the coverage runtime is setup, instrumenting a program is achieved
with a |gcvins| command like::

  gnatcov instrument --level=<> <units-of-interest>
    [--dump-trigger=<>] [--dump-channel=<>]


:option:`--dump-trigger` and :option:`--dump-channel` select the execution
point at which the output of coverage data should be injected and the output
medium, respectively, with a variety of possibilities to select depending on
the runtime environment capabilities.  This might involve two kinds of
instrumentation: one on main units to output the coverage data after it has
been gathered, and one on the units of interest to collect the coverage data
in the first place.

After instrumentation, building the program using instrumented source is
achieved with a :command:`gprbuild` command like::

  gprbuild -P<project> --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts_full.gpr

``GPR_PROJECT_PATH`` should be set to designate the directory where the
coverage runtime has been installed if that is not a place where
:command:`gprbuild` would search by default (such that the GNAT installation
prefix).

The production of coverage data is then simply achieved by executing the
program.

A general property of note here is that instrumentation works
within the context of an existing project structure, just adding alternate
versions of the sources in strategically chosen places. We do not replicate
the entire project tree, not even project files. Instead, the
:option:`--src-subdirs` and :option:`--implicit-with` options combine together
to allow the entire instrumented build to proceed with the original project
files.

Producing reports from traces (``gnatcov coverage``)
----------------------------------------------------

The production of a coverage report can most often be done directly from one
or more traces with |gcvcov|, like::

  gnatcov coverage --level=<> --annotate=<>
    [<units-of-interest>] | [--routines=@<symbols-list>] <trace1> <trace2> ...

- :option:`--annotate` specifies the desired output report format
  (:option:`=report` for a synthetic list of coverage violations,
  :option:`=xcov` for annotated sources in text format, :option:`=dhtml`
  for annotated sources in HTML format, with colors, sortable columns, and
  per-project indexes);

- The :option:`<units-of-interest>` options convey the set of units for which
  the analysis is to be performed;

- :option:`--routines` is specific to the object level criteria, and
  optional in this case. This conveys the set of object symbol names
  on which the analysis should focus, if any.

For source coverage criteria, this interface is the same with source and
binary traces. The presence of multiple traces on the command line requests
the production of a report which combines the coverage achieved by all the
corresponding executions, a process we refer to as :term:`coverage
consolidation`. Consolidation can also be performed using partial/intermediate
result files called :term:`coverage checkpoints`, as explained in more details
in the :ref:`consolidation` chapter of this manual.

Example session, from sources to coverage analysis
==================================================

We start from the very basic Ada package below, with a spec and body in source
files named ``ops.ads`` and ``ops.adb``, exposing a set of very basic named
operations over ``Integer`` objects:

.. code-block:: ada

   package Ops is
     type Op_Kind is (Increment, Decrement);
     procedure Apply (Op : Op_Kind; X : in out Integer);
   end Ops;

   package body Ops is
     procedure Apply (Op : Op_Kind; X : in out Integer) is
     begin
        case Op is
           when Increment => X := X + 1;
           when Decrement => X := X - 1;
        end case;
     end Apply;
   end Ops;

We will analyse the statement coverage achieved by the sample unit
:term:`test driver` below, in ``test_inc.adb``, which exercises the
``Increment`` operation only:

.. code-block:: ada

   with Ops;
   procedure Test_Inc is
     X : Integer := 4;
   begin
     Ops.Apply (Ops.Increment, X);
     pragma Assert (X = 5);
   end Test_Inc;


We will illustrate two basic use cases, one using binary traces produced by
GNATemulator for a cross target, and one using source traces for a native
environment.

Assuming we start from a temporary working directory, with the *ops* sources
in an ``opslib`` subdirectory and the *test* sources in a ``tests``
subdirectory, we will rely for both cases on a couple of project files in the
common working directory:

.. code-block:: ada

  -- code.gpr
  project Code is
    for Source_Dirs use ("opslib");
    for Object_Dir use "obj-" & Project'Name;
  end Code;

  -- tests.gpr
  with "code.gpr";
  project Tests is
    for Source_Dirs use ("tests");
    for Object_Dir use "obj-" & Project'Name;

    for Main use ("test_inc.adb");
  end Tests;


If you wish to experiment with both modes, you should start from separate
working directories to prevent possible intereferences of artifacts from one
mode on the other, as the two schemes are not designed to work together.

Example production of binary traces for a bareboard target
----------------------------------------------------------

We first use the GNAT Pro toolset for ``powerpc-elf`` to build, using
:command:`gprbuild` as follows::

   gprbuild -p --target=powerpc-elf --RTS=zfp-mpc8641 -Ptests.gpr
    -cargs:Ada -gnata -cargs -g -fpreserve-control-flow -fdump-scos

In this particular case:

- :option:`-p` queries the creation of the "obj" object directory if it
  doesn't exist. This is where the object, ALI, and executable files will
  reside.

- :option:`--target` and :option:`--RTS` tell :command:gprbuild which target toolchain
  and runtime library to use. Here, powerpc-elf and a zero-footprint library
  tailored for the ``mpc8641`` GNATemulator board.

- :option:`-Ptests.gpr` :option:`test_inc.adb` designate the project file and
  the main unit to build.

- :option:`-cargs:Ada` sets the Ada specific compilation option and
  :option:`-cargs` sets the more general ones in accordance with the
  guidelines stated earlier.

The build command produces a ``test_inc`` executable in the object
subdirectory, and now we can do::

  gnatcov run --target=powerpc-elf obj-tests/test_inc

This executes the program within the instrumented execution environment, via
GNATemulator, producing a ``test_inc.trace`` binary trace in the current
directory.

Example production of source traces for a native environment
-------------------------------------------------------------

Assuming the coverage runtime is available, the first step consists in
instrumenting the test main program together with its "code" dependency. Here
we request the output of coverage data when the program exits::

    gnatcov instrument -Ptests.gpr --level=stmt --dump-trigger=atexit

Building the instrumented program would then go like::

    gprbuild -f -p -Ptests.gpr --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts_full.gpr

After which we can simply execute the test program as in::

  obj-tests/test_inc

to produce a ``test_inc.srctrace`` source trace in the current directory.

Example production of a coverage report
---------------------------------------

We can analyse the coverage achieved by either execution using
|gcvcov|, for example with::

  gnatcov coverage --level=stmt --annotate=xcov <trace> -Ptests.gpr

... where ``<trace>`` would be either the source or the binary trace produced
previously. Here, we request:

- A source *statement coverage* assessment with :option:`--level=stmt`,

- An annotated source report in text format with :option:`--annotate=xcov`,

- For the complete set of units involved in the executable, per
  :option:`-Ptests.gpr` and no specification otherwise in the project files.

This produces annotated sources in the project's object directory,
with ``ops.adb.xcov`` quoted below:

.. code-block:: ada

  examples/starter/src/ops.adb:
  67% of 3 lines covered
  Coverage level: stmt
   1 .: package body Ops is
   2 .:    procedure Apply (Op : Op_Kind; X : in out Integer) is
   3 .:    begin
   4 +:       case Op is
   5 +:          when Increment => X := X + 1;
   6 -:          when Decrement => X := X - 1;
   7 .:       end case;
   8 .:    end Apply;
   9 .: end Ops;

The analysis results are visible as ``+`` / ``-`` annotations on source lines,
next to the line numbers. The results we have here indicate proper coverage of
all the statements except the one dealing with a ``Decrement`` operation,
indeed never exercised by our driver.

The command actually also produces reports for ``ops.ads`` and
``test_inc.adb``, even though the latter is not really relevant. Focus on
specific units, excluding the test driver from the analysis closure for
example, can be achieved by adding a ``Coverage`` package to the *tests*
project file, by using :option:`--scos=obj-code/ops.ali` instead of
:option:`-P`, or by adding ``--projects=code.gpr`` to the command line so
units from only this subproject are considered of interest. For source traces,
this could also be incorporated as part of the instrumentation step, as there
is no point in instrumenting the test units for their own coverage
achievements.

Going Further
=============

Beyond the simple cases sketched previously, |gcp| supports advanced
capabilities available for both source and object coverage criteria.

Two examples are *coverage consolidation*, computing results for a set of
execution traces, and *exemption regions*, allowing users to define code
regions for which coverage violations are expected and legitimate.

As another example, the handling of libraries with the instrumentation scheme
requires particular care to prevent re-instrumentation of a library together
with every different test that exercises part of it.

The following chapters in this manual provide many more details on such
topics.
