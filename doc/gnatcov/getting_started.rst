***************
Getting Started
***************

General notions
===============

|gcp| is a code coverage analysis tool offering support for a range of
coverage metrics and output formats associated with powerful
:dfn:`consolidation` features letting users assess the combined
coverage achievements of multiple program executions.

The set of compilation units for which a user needs to assess coverage
is commonly designated as the set of :dfn:`units of interest`. This is a
central notion to many of the tool's operations, conveyed by the user
through command line switches or GPR project file attributes. Units of
interest typically include the code under test in a campaign, as
opposed to the sources of the test harness infrastructure.

After one or several program executions, the tool computes
:dfn:`coverage metrics` for a given set of units out of :dfn:`coverage
trace` data produced by the executions with the assistance of an
*instrumentation* mechanism. The primary mode offered by |gcp|
performs :dfn:`source instrumentation`, where the tool produces a
modified version of the program sources to keep track of coverage
relevant facts along with the program control flow, and output
the coverage data when the program terminates. We call :dfn:`source
traces` the coverage traces produced by programs instrumented this
way.

The set of metrics that the tool can assess from source traces
corresponds to the set defined by the |do| certification standard
for civil avionics, that is:

- :dfn:`Statement Coverage`, where the tools assesses the coverage
  status (executed or not) of source statements such as a variable
  assignment or a subprogram call;

- :dfn:`Decision Coverage`, which, in addition to statement coverage,
  evaluates whether Boolean expressions (*decisions* in |do|
  parlance) have been exercised both True and False, then

- :dfn:`Modified Condition/Decision Coverage`, commonly known as
  *MCDC*, which requires testing particular variations of individual
  Boolean operands (*conditions* in |do| parlance) within decisions.

All these metrics are defined with respect to source level entities
(statements, decisions, conditions), and we commonly refer to such
assessments as :dfn:`source coverage analysis`. Individual statements,
decisions, or conditions within the scope of an analysis are referred
to as :dfn:`Source Coverage Obligations` or :dfn:`SCOs`.

Workflow overview
=================

Figure :numref:`fig-flow_srctraces` depicts the workflow involved in
computing coverage based on source instrumentation.

A setup step is first needed to install a tiny runtime library used by
the instrumented sources to register coverage facts and output them
eventually. This runtime is parameterized for the project at hand,
depending on the set of languages involved (Ada, C, C++) and on the
kind of target for which the code will be compiled (native, cross with
OS, or bareboard). Once the setup is done, users proceed with
instrument / build / execute / analyze cycles to produce traces and
compute coverage results.

The setup, instrumentation and analysis steps are all driven by a
|gcv| command line tool which exposes a subcommand for each kind of
operation; hence |gcvstp| for the setup step, |gcvins| to instrument
and |gcvcov| to compute metrics from traces.

GPR project files and associated command line switches are used to let
the tool know about the project sources and to convey the units of
interest. Consolidation can be performed by aggregating multiple traces
directly, or internal representations of partial coverage results
stored in files that we call :dfn:`Coverage Checkpoints`.

.. _fig-flow_srctraces:
.. figure:: fig_flow_srctraces.*
  :align: center

  Source instrumentation based Coverage analysis overview

|gcv| also provides an alternate workflow that consists in integrating
seamlessly into an existing build process possibly not involving the use of
gprbuild. This is provided as an experimental feature and is available for
C/C++, compiling with gcc/g++ on a linux host. The figure
:numref:`fig-flow_integrated_instr` depicts this workflow that is more
thoroughly detailed in the section :ref:`integrated_instr`.

.. _fig-flow_integrated_instr:
.. figure:: fig_flow_integrated_instr.*
  :align: center

  Integrated instrumentation based Coverage analysis overview

A simple example
================

Here we first introduce a very simple example software project
structure then demonstrate one basic analysis workflow for this
project with actual commands.

The examples provided in most of this manual assume a native
configuration and the process is similar cross environments. Typical
variations would touch switches controlling where and when coverage
trace data is output by the instrumented programs, and default values
for such switches are selected by the tool based on
:cmd-option:`--target` and :cmd-option:`--RTS` arguments at setup
time.


Sample project
--------------

The sources for this examples are provided under the
`share/examples/gnatcoverage/doc/getting_started/` directory of the GNATDAS
distribution.

We consider an Ada package providing a set of elementary operations
over ``Integer`` objects, with a spec and body in source files named
``ops.ads`` and ``ops.adb``:

.. code-block:: ada

   -- ops.ads
   package Ops is
     type Op_Kind is (Increment, Decrement);

     procedure Apply (Op : Op_Kind; X : in out Integer);
   end;

.. code-block:: ada

   -- ops.adb
   package body Ops is
     procedure Apply (Op : Op_Kind; X : in out Integer) is
     begin
        case Op is
           when Increment => X := X + 1;
           when Decrement => X := X - 1;
        end case;
     end;
   end;

We will analyze the coverage achieved by the sample unit :term:`test
driver <Test Driver>` below, in ``test_inc.adb``, which exercises the
``Increment`` operation only:

.. code-block:: ada

   -- test_inc.adb
   with Ops;
   procedure Test_Inc is
     X : Integer := 4;
   begin
     Ops.Apply (Ops.Increment, X);
     pragma Assert (X = 5);
   end;


Assuming a working directory, with the *ops* sources in an ``opslib``
subdirectory and the *test* sources in a ``tests`` subdirectory, we
will use a couple of project files in the common working directory:

.. code-block:: ada

  -- code.gpr
  project Code is
    for Source_Dirs use ("opslib");
    for Object_Dir use "obj-" & Project'Name;
  end Code;

.. code-block:: ada

  -- tests.gpr
  with "code.gpr";

  project Tests is
    for Source_Dirs use ("tests");
    for Object_Dir use "obj-" & Project'Name;

    for Main use ("test_inc.adb");
  end Tests;


Setup, Instrument, Build, Execute, Analyze
------------------------------------------

The instrumentation step that follows assumes that the original program
is well formed. A simple way to verify this is to build the non instrumented
version first. For our example, this would be::

   gprbuild -f -p -Ptests.gpr

We then first set up the instrumentation context, providing a local
*prefix* location where the runtime and default parameters for future
commands are going to be installed::

   gnatcov setup --prefix=/path/to/gnatcov-rts

Letting further commands know about the *prefix* location is achieved
by adding ``<prefix>/share/gpr`` to the ``GPR_PROJECT_PATH``
variable. In a Unix like environment, this would be:

.. code-block:: sh

   export GPR_PROJECT_PATH=$GPR_PROJECT_PATH:/path/to/gnatcov-rts/share/gpr

This will both let the ``gprbuild`` command below locate the
``gnatcov_rts.gpr`` project file, and the |gcvins| command find
default parameter values.

In addition, when using shared libraries, it is necessary to let the
environment know about the coverage runtime's own shared libraries. The
following command achieves this in a Unix like environment:

.. code-block:: sh

   export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/path/to/gnatcov-rts/lib

The following command performs the corresponding action on Windows systems:

.. code-block:: bat

   set PATH=%PATH%;C:\path\to\gnatcov-rts\bin\

Instrumentation is performed by a simple |gcvins| command::

   gnatcov instrument -Ptests.gpr --level=stmt

The use of ``tests.gpr``, not ``code.gpr``, at this step is important
as it lets the instrumenter know about the main subprogram, which
needs to be processed specially to dump coverage data.

Building the instrumented program then goes like::

   gprbuild -f -p -Ptests.gpr \
      --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts.gpr

This is the same command as for the regular build, with a couple
of additional switches to:

  * Instruct the builder to search for the instrumented versions of the sources
    (``--src-subdirs``). Note that, due to the way ``--src-subdirs`` works in
    ``gprbuild``, even though |gcvins| creates directories with names derived
    from ``*-gnatcov-instr``, the argument to pass to GPRbuild must always be
    exactly ``--src-subdirs=gnatcov-instr``.

  * Provide visibility to the builder over the coverage runtime
    referenced by the instrumented sources (``--implicit-with``).

Executing the test program in its native environment, as in::

  obj-tests/test_inc

then produces a ``test_inc-<stamp>.srctrace`` source trace file in the
current directory. The ``-<stamp>`` suffix is intended to prevent
clashes in case of concurrent executions of the program in the same
directory. It can be controlled in a variety of ways from the
instrumentation command line, documented in the :ref:`instr-tracename`
section of this manual.

Analysis of the coverage achieved by previous executions is done with
|gcvcov| commands. For our example use case, this could for instance be::

  gnatcov coverage --level=stmt --annotate=xcov test_inc*.srctrace -Ptests.gpr

Here, we request:

- A source *statement coverage* assessment with :cmd-option:`--level=stmt`,

- An annotated source report in text format with :cmd-option:`--annotate=xcov`,

- For the complete set of units involved in the executable with
  :cmd-option:`-Ptests.gpr`.

This produces annotated sources in the project's object directory,
with ``ops.adb.xcov`` quoted below:

.. code-block::

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
``test_inc.adb``, even though the latter is not really relevant. Focus
on specific units can be achieved by providing a more precise set of
units of interest at this step, for example by adding
``--projects=code.gpr`` to the command line, or setting dedicated attributes
in the project files themselves. See the :ref:`sunits` chapter for
details on this aspect of the procedure.

Going Further
=============

Each of the steps involved in the process overview presented previously
is described in detail in a specific chapter of this manual. The most
important ones are:

- :ref:`src_traces`

- :ref:`sunits`

- :ref:`scov`

- :ref:`consolidation`

:ref:`exemptions` is also worth noting here, a mechanism allowing
users to define code regions for which coverage violations are
expected and legitimate.

The known limitations of the tool are outlined in section
:ref:`instr-limitations`.


Conventions used in the rest of this manual
===========================================

- A number of example commands include a :cmd-option:`--level=`:cmd-option:`<>`
  switch, which conveys a target coverage criterion when needed. ``<>`` is a
  placeholder for an actual level supported by the tool in this case, such as
  ``stmt``, ``stmt+decision``, or ``stmt+mcdc`` for source criteria.

- Example command lines might also include as :cmd-option:`<units-of-interest>`
  placeholder, which represents a set of switches conveying the set of units
  for interest for source coverage assessments. GPR project files provide the
  most elaborate mechanisms for this purpose and the :ref:`sunits` chapter
  describes all the available options.
