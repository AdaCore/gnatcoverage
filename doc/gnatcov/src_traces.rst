.. _src_traces:

#####################################
Producing source traces with |gcvins|
#####################################

The production of :term:`source traces` is performed by an instrumented
version of the original program running in its regular execution
environment. A |gcvins| command produces alternative sources for units of
interest, with additional data structures and code statements solely aimed at
tracking coverage related information. Coverage data is output in accordance
with a user selectable policy, for example upon exit of the program, for which
the program main unit gets instrumented as well.

To gather coverage data while the program is running and to output this data
in the form of a source trace file eventually, the instrumented code relies on
common data types and subprograms provided by a :term:`coverage runtime`
library.  The first thing to do for a given project is then to build and
install the coverage runtime so it becomes available to the instrumented
sources afterwards, as we will describe in the :ref:`instr-rts` section
of this chapter.

Once the coverage runtime is setup, the simplest possible use of the
instrumentation is on entire programs at once, with their main unit(s)
included. The process essentially goes like:

#. Instrument, both units-of-interest and main units;
#. Build the instrumented code;
#. Execute the program to produce a trace.

|gcp| also allows partial instrumentations to be combined together, which
we will describe in the :ref:`instr-compose` part.

The instrumenter is generally careful not to introduce constructs specific to
a version of Ada more recent than the original one, only assuming that this is
at least Ada 95. In order to support Pure or Preelaborable units, we currently
assume that compilations are performed with a GNAT Pro toolchain. Parts of the
effects reach into the coverage runtime, so this also concerns application
programs which do not have so categorized units.

|gcp| supports two direct ways of outputting coverage data from the program:

- Produce a trace file directly, or

- Output a base64 encoded version of the data on standard output, using
  ``Ada.Text_IO``. Coverage data is emitted between markers and a trace file can
  be produced offline from a captured output thanks to the |gcvxtr| command.

The |gcvins| command allows instrumenting main units according to a specified
mode, with a choice of possible insertion points. It also features a *manual*
mode so users can implement a custom strategy, which only requires that the
output format abides by one of the two variants we support.

Thread safety on accesses to coverage data structures is achieved by writing
at most one status Boolean at a time in data structures voluntarily *not*
packed.

.. _instr-rts:

Setting up the :term:`coverage runtime` library
===============================================

|gcp| is shipped with the sources of the coverage runtime and with a companion
project file. The |gcvstp| command helps automating the build and installation
of this project.

Just like :command:`gprbuild`, |gcvstp| accept the :option:`--RTS`,
:option:`--config` and :option:`--target` command line options: you need to
build the coverage runtime with the same toolchain and runtime as the ones used
to build the application code. For instance:

.. code-block:: sh

   # For a native project
   gnatcov setup

   # For a project that targets ARM ELF with the light-stm32f4 runtime
   gnatcov setup --target=arm-eabi --RTS=light-stm32f4

By default, |gcvstp| installs the coverage runtime in the same prefix as the
selected toolchain (just like :command:`gprinstall`). In order to install it in
another location, pass the :option:`--prefix` option:

.. code-block:: sh

   gnatcov setup --prefix=/usr/local


Multiple runtimes in the same prefix
------------------------------------

It is sometimes convenient to install multiple times the coverage runtime in
the same prefix, for instance when working on a project that runs both on the
native platform and on an embedded target while the two toolchains are also
installed in the same prefix. It is possible to make |gcvstp| install the
coverage runtime under different project names: the default is ``gnatcov_rts``,
and using the :option:`--install-name` option changes it.

.. code-block:: sh

   # Install the coverage runtime both for native projects (gnatcov_rts_native)
   # and for ARM ELF/light-stm32f4 projects (gnatcov_rts_stm32f4).
   gnatcov setup --install-name=gnatcov_rts_native
   gnatcov setup --target=arm-eabi --RTS=light-stm32f4 \
     --install-name=gnatcov_rts_stm32f4

When building instrumented projects later with :command:`gprbuild`, you then
have to select the appropriate coverage runtime with the
:option:`--implicit-with` switch:

.. code-block:: sh

   # Build the instrumented project with the native toolchain and the
   # corresponding coverage runtime.
   gprbuild -Pmy_project \
     --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts_native

   # Now do the same, but for ARM ELF/light-stm32f4
   gprbuild -Pmy_project --target=arm-eabi --RTS=light-stm32f4 \
     --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts_stm32f4


Language restrictions
---------------------

By default, |gcvstp| builds and installs the coverage runtime with Ada and C
units. When using |gcp| with a C-only toolchain, it is necessary to build the
coverage runtime without its Ada units. The :option:`--restricted-to-languages`
option allows that:

.. code-block:: sh

   # Build and install the coverage runtime with C units only
   gnatcov setup --restricted-to-languages C

   # Build and install the coverage runtime with both Ada and C units (the
   # default).
   gnatcov setup --restricted-to-languages Ada,C

Note that for now, only Ada and C are supported. The core runtime is
implemented in C, so the C language must be enabled in all cases.


.. TODO (U211-014): Document:

   * the project positional argument (to install an extending coverage runtime
     project).

   * dump options and their interactions with "gnatcov instrument"


Instrumenting programs
======================

Instrumentation is performed upfront for an intended strictest coverage
criterion on a given set of :term:`units of interest`. The production of a
coverage report afterwards might restrict the report to a subset of those
units, or lower to a less strict criterion.
Instrumenting programs is achieved with |gcvins| commands, which might
involve two distinct kinds of operations:

- Modify the code in units-of-interest so the program records, while it is
  running, facts of relevance to the coverage metrics to be assessed,

- Modify the main unit(s) to output the so gathered coverage data to
  an externally readable channel, typically either a source trace file or some
  communication port.

|gcvins| command line
---------------------

As for other commands, help on the command line interface is displayed
by ``gnatcov instrument --help``. The general sysopsis is as follows::

  gnatcov instrument --level=<> <units-of-interest> [OPTIONS]

:option:`--level` states the strictest criterion that this instrumentation
will allow assessing afterwards and the :option:`<units-of-interest>` switches
specify the set of units for which such assessment will be possible. The
latter may only resort to project file facilities, as described in the
:ref:`passing_gpr` section of this manual. Projects marked ``Externally_Built``
in the closure are not instrumented or otherwise modified.

A few :option:`[OPTIONS]` allow controlling the instrumentation of main units,
if any are designated by the root project:

:option:`--dump-trigger`
   selects the execution point at which the output of
   coverage data should be injected in main units. This is ``manual`` by
   default, leaving to users the responsibility to emit the coverage data as
   they see fit. Other possible choices are ``atexit``, ``main-end`` and
   ``ravenscar-task-termination``.

:option:`--dump-channel`
   selects the mechanism used to output coverage data at the selected triggering
   point, if any. The possible choices are ``bin-file``, to create a source
   trace file, or ``base64-stdout`` to emit a base64 encoded version of the data
   through ``GNAT.IO``. ``bin-file`` is the default.

:option:`--externally-built-projects`
   instructs the instrumenter to look into projects marked as externally built
   when computing the list of units of interest (they are ignored by default),
   for the sole purpose of instrumenting mains.

In addition, for trace files produced automatically from a ``bin-file``
dump-channel, the ``--dump-filename-<>`` family of switches provides control
over the name of trace files. See :ref:`instr-tracename` for more details on
the default behavior and possibilities to alter it.


Output strategies for main units
--------------------------------

The choice of a dump-trigger/dump-channel pair for main units depends on the
runtime environment available to your program.

For a native program with access to a full Ada runtime and the associated
coverage runtime, ``bin-file`` is the recommended channel as it produces a
trace in the most direct manner and separates the trace data from the regular
output. ``atexit`` is a natural triggering choice in this case, as it takes
care of outputting the data automatically at a point where we know the program
is not going to execute more, regardless of how or why the program exits.

The ``main-end`` alternative simply inserts the calls at the end of the main
subprogram bodies, which may be bypassed if the program exits abruptly, or
miss data if the program has tasks not terminated when execution of the main
subprogram/thread reaches its end.

For more restricted environments where, say, there is limited file IO
available to the program, a ``base64-stdout`` kind of output is needed in
association with the restricted coverage runtime.

If none of the available automatic triggering option works out well, full
control is offered by the ``manual`` policy where the instrumenter doesn't
actually add any code to main units for emitting the collected coverage
data. You will have to emit this data somehow to allow analysing coverage
afterwards, still, and can of course experiment with other possibilities just
to get examples of possible ways to proceed.

.. _instr-tracename:

Controlling trace file names
----------------------------

When an instrumented program produces a trace file through a ``bin-file``
dump-channel, the file is by default created in the current working directory
at the data output point (for example, at exit time for an ``atexit``
dump-trigger), and named as ``<ename>-<istamp>-<pid>-<estamp>.srctrace``,
where:

- ``<ename>`` is the executable name,

- ``<istamp>`` is the instrumentation time stamp, representing the time at
  which the instrumentation took place,

- ``<pid>`` is the execution process identifier,

- ``<estamp>`` is an execution time stamp, representing the time at which
  coverage data was written out to the file.

The ``<estamp>`` and ``<pid>`` components are intended to ensure that parallel
executions of the program from the same working directory write out to
different files. The ``<istamp>`` component allows distinguishing traces
issued from different versions of the program. These three components are
expressed as hexadecimal integers to limit the growth of file name lengths.

This default behavior can be influenced in several manners. First:

* The :option:`--dump-filename-prefix` switch to |gcvins| requests replacing
  the ``<ename>`` component by the switch argument;

* The :option:`--dump-filename-simple` switch requests the removal of the
  variable components (stamps and pid), so only the ``<ename>`` component
  remains or the replacement provided by :option:`--dump-filename-prefix` if
  that switch is also used.


The use of a specific location for the file, or of a specific file name can be
requested at run time by setting the ``GNATCOV_TRACE_FILE`` variable in the
program's environment.

If the variable value ends with a ``/`` or ``\`` character, this value is
interpreted as the name of a directory where the trace file is to be produced,
following the rules we have just described for the file base name. The
directory reference may be a full or a relative path, resolved at the trace
file creation point and expected to exist at that time.

If the variable value does *not* end with a ``/`` or ``\`` character, the
value is used directly as the name of the file to create. This name may hold a
path specification, full or relative, also resolved at the trace file creation
point and the directories involved are expected to exist at that time.

For specific needs of programs wishing to output to different places from
within the same environment, the variable name for a program can actually be
tailored by passing a :option:`--dump-filename-env-var` switch to |gcvins|,
providing the variable name to use.

.. _instr-limitations:

|gcvins| limitations
--------------------

There are situations and code patterns not handeled correctly by |gcvins|.
Below are listed the limitations associated with general Ada sources.
Coverage of SPARK sources require additional considerations, detailed in
section :ref:`spark_instr`.

Unsupported source constructs
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There are a few language constructs that |gcvins| doesn't handle properly.
The tool emits a warning when it encounters such cases and the corresponding
code is not instrumented. Source coverage obligations are still emitted, so
the unsupported constructs will appear as ``not covered`` in the report.

The list of unsupported constructs is as follows:

* Generic expression functions,
* Generic null procedures,
* Recursive expression functions which are primitives of some tagged type,
* Expression functions which are primitives of their return type, when it is a
  tagged type.

The simplest way to work around the limitation concerning expression functions
is to turn them into regular funtions, by giving them a proper body,
containing a single return statment with the original expression.
Otherwise it is possible to exempt those constructs (see :ref:`exemptions`)
and/or perform a manual coverage analysis for these special cases.

Global source traces limitations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There are also a few limitations concerning the source trace workflow as a
whole:

- Separate analysis of generic package instances is not supported,
- Preprocessing directives are ignored by the source instrumenter.

Toolchain-specific limitations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

With GNAT versions from 7.1 to 7.3, compiling with optimization will result in
coverage violations on all statement obligations associated with expression
functions. Explicitely disabling optimization (with ``-O0`` for instance) will
resolve this issue.

.. _instr-build:

Building instrumented components
================================

Compared to a regular build, the intermediate instrumentation process requires
two specific actions:

- For the units which have been instrumented (as main units or declared
  of-interest to coverage instrumentation time), arrange to use the
  instrumented sources instead of the original ones; and

- Provide the instrumented code with access to the coverage runtime support.

Since release 20, our GPRbuild builder incorporates features allowing a direct
reuse of a project hierarchy without replication of the directory structure,
not even modification of the project files.

For each project in the closure of-interest, the instrumentation generates the
alternative sources in the ``gnatcov-instr`` subdirectory of the project's
object directory.  Giving priority to this subdir when it exists is achieved
by passing a :option:`--src-subdirs` switch to :command:`gprbuild`, naming
that particular relative subdirectory.

Then :command:`gprbuild` now supports a :option:`--implicit-with` option which
requests processing every project as if it started with a ``with`` statement
for a given project, which we can use to designate the coverage runtime
project file so all the compiled code gets access to the support packages.

The build of instrumented components then proceeds almost exactly as a regular
one, only adding :option:`--src-subdirs=gnatcov-instr` and
:option:`--implicit-with=`:option:`<gnatcov_rts_gpr>` to the build options,
where :option:`<gnatcov_rts_gpr>` would be the coverage runtime project file
setup beforehand for the project, as described previously in this
chapter. This project file could be referred to with a full path specification,
or with a simple basename if the ``GPR_PROJECT_PATH`` environment variable is
updated to designate the directory where the project file is located, which
would be the ``share/gpr`` subdirectory of the runtime installation tree.

While the scheme relies on the use of GNAT project files, it does not
absolutely require :command:`gprbuild` to build the instrumented programs,
even though we have augmented that builder with a few features to make that
process very efficient and straightforward.

Extracting a trace from standard output
=======================================

With the :option:`base64-stdout` channel, coverage data is emitted with
``GNAT.IO`` on the program's standard output stream. The actual base64 encoded
data is framed by start/end-of-coverage-data markers and |gcp| provides the
|gcvxtr| command to extract this data from a captured output and create a trace
file offline (outside of the program's execution context). The extraction
command line simply is::

  gnatcov extract-base64-trace <captured-output> <output-trace-file>

The captured output may be used directly, there is no need to first extract
the trace data section.

.. _instr-compose:

Composed instrumentation
========================

To prevent unnecessary re-instrumentation and re-build of components which
don't change, |gcp| allows partial instrumentations to be combined together. A
common use case would be the testing of library components, where the library
doesn't change and its coverage needs to be assessed incrementally as new
tests get developed.

In such situations, the process would become something like:

#. Setup or reuse a separate project file for the library, which normally
   wouldn't have any main unit;
#. Instrument the library using this project as the root project;
#. Build the instrumented library;

Then for each new test:

#. Setup or reuse a separate project file for the test, designating the main
   unit if you wish to leverage the instrumenter's ability to insert the
   coverage coverage data output code. Setup a dependency from this project on
   the library project, with an ``Externally_Built`` attribute set to ``"True"``;
#. Instrument the testing code main unit alone;
#. Build a program combining the library (instrumented for coverage
   measurement) and the testing code (instrumented to output the gathered
   coverage data);
#. Execute the program to produce a trace.

The section :ref:`examples_src_traces` illustrates such a use case.

.. _other_languages_instr :

Instrumentation of a multi-languages project
============================================

The |gcp| instrumentation mode supports Ada, and C. As C support is still at its
beta stage, it is disabled by default. To activate it, add the
:option:`--restricted-to-languages=C,Ada` switch to your command line.

Note that, as of now, the C instrumented code still uses the Ada coverage
runtime, described in the section :ref:`instr-rts`. To interface with this
runtime, the instrumenter thus generates Ada files that must be compiled with
your project. For that reason, "Ada" needs to be included in the Languages
attribute of your gpr project file (|gcv| will emit a warning at instrumentation
time if this is not the case).

Instrumentation operates on the preprocessed code, which means that coverage
obligations that come from a macro expansion (and further obligations on the
same line) will have inaccurate column numbers.

.. _spark_instr :

Instrumentation and coverage of SPARK code
==========================================

The instrumentation of a SPARK project requires an additionnal step in order
to make the compiler accept the instrumented code. Additionally, some parts of
SPARK sources are not processed by the instrumenter, and thus will not have
any coverage obligation attached to them.

Inhibiting SPARK related pragmas
---------------------------------

SPARK introduces a certain number of pragmas and aspects to aid the developper
in writting program contracts and guiding the automatic provers. These are only
useful for static proof purposes, and are not used when assessing the coverage
of a project during testing. As such, the instrumenter ignores those
pragmas/aspects, and the compiler must be instructed to disregard them when
processing instrumented sources.

To do so, the simplest option is to pass a configuration pragma file which
inhibits each of the above pragmas when building the project. Such
a configuration pragma file can be found at
``examples/support/instrument-spark.adc`` in the installation tree of |gcv|.
Its contents are:

.. code-block:: ada

  --  This is the list of global restrictions to be used when building
  --  instrumented spark code.
  --
  --  We need to enforce such restrictions, as gnatcov instrumentation generates
  --  constructs incompatible with a SPARK_Mode compilation.

  pragma Ignore_Pragma (SPARK_Mode);
  pragma Ignore_Pragma (Refined_State);
  pragma Ignore_Pragma (Abstract_State);
  pragma Ignore_Pragma (Global);
  pragma Ignore_Pragma (Depends);
  pragma Ignore_Pragma (Part_Of);
  pragma Ignore_Pragma (Initializes);
  pragma Ignore_Pragma (Refined_Global);
  pragma Ignore_Pragma (Refined_Depends);

The configuration pragma file can be passed to the compiler either by
specifying it on the gprbuild command line with the ``-gnatec`` switch::

  gprbuild -Pproject --src-subdirs=gnatcov-instr --implicit-with=<path-to-runtime> -cargs:Ada -gnatec=instrument-spark.adc

or by way of a ``Global_Configuration_File`` project file attribute,
possibly controlled by a scenario variable as in:

.. code-block:: ada

  type mode is ("prod", "coverage");
  BUILD_MODE : mode := external ("BUILD_MODE", "prod")

  package Compiler is
    case BUILD_MODE is
      when "coverage" => for Global_Configuration_File use "instrument-spark.adc";
      when "prod"     => null;
    end case;
  end Compiler;

and then building with::

  gprbuild -Pproject --src-subdirs=gnatcov-instr --implicit-with=<path-to-runtime> -XBUILD_MODE=coverage

For SPARK projects for which unit testing is performed through GNATtest,
see :ref:`gnattest_spark_instrument` for instructions on how to pass the
configuration pragma file when building the test harness.

Coverage obligations for SPARK code
-----------------------------------

Some parts of SPARK sources do not necessarily generate executable code when
compiled, and are mainly used to aid the proof of the program.
Computing coverage for such source regions isn't meaningful and are thus
ignored by the instrumenter. This means that those regions will not have any
coverage obligation attached to them in the coverage reports.

The concerned pieces of code are notably:

- any entity that is ``Ghost``
- any contract (``Pre``/``Post``/``Contract_Cases``/``Loop_Invariant``)

Note that since no coverage obligations are emitted for such source
constructs, they will not appear in the coverage reports even if assertions
are enabled and the assertion policy enables the compilation of ghost code.

.. _examples_src_traces:

Example use cases
=================

Whole program instrumented at once, cross configuration, base64 output
----------------------------------------------------------------------

Here we will consider examining the coverage achieved by the execution of the
very basic sample program below, assuming the existence of a ``Sensors``
source unit providing access to some sensor values.

.. code-block:: ada

  with Sensors; use Sensors;
  with Ada.Text_IO; use Ada.Text_IO;

  procedure Monitor is
     Sensor_Value : Integer;
  begin
     for Sensor_Index in Sensor_Index_Range loop
        Sensor_Value := Sensors.Value (Sensor_Index);
        Put ("Sensor(" & Sensor_Index'Img & ") = " & Sensor_Value'Img & " ");
        if (Sensor_Value > 1000) then
           Put_Line ("!Alarm!");
        else
           Put_Line ("!Ok!");
        end if;
     end loop;
  end;

We will consider a cross target environment, say PowerPC-VxWorks, using Real
Time Processes hence an :option:`rtp` Ada runtime library. We will assume we
don't have a filesystem at hand, so will rely on the base64 encoded output of
trace data to standard output.


Setting up the coverage runtime
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We just "build" the runtime library project as we would build a regular
program for our target configuration, specifying the target name and the
intended base Ada runtime library.

For our intended target environment, this would be something like::

  # Copy the sources into a fresh local place for the build:
  cp -rp <gnatcoverage-install>/share/gnatcoverage/gnatcov_rts <gnatcov_rts-build-dir>

  # Build and install the library to a place of our choice. Pick gnatcov_rts.gpr as
  # we won't be emitting source trace files directly:

  cd <gnatcov_rts-build-dir>
  gprbuild -Pgnatcov_rts.gpr --target=powerpc-wrs-vxworks7r2 --RTS=rtp -f -p

  rm -rf <gnatcov_rts-ppc-install-dir>
  gprinstall -Pgnatcov_rts.gpr --target=powerpc-wrs-vxworks7r2 --RTS=rtp \
    -p --prefix=<gnatcov_rts-ppc-install-dir>

  # Allow references to the coverage runtime project from other project files:
  export GPR_PROJECT_PATH=<gnatcov_rts-ppc-install-dir>/share/gpr

Instrument and build
^^^^^^^^^^^^^^^^^^^^

We setup a ``monitor.gpr`` project file for our program, where we

- Provide the main unit name, so it can be instrumented automatically, and...

- State the target configuration name and Ada runtime library so we won't have
  to pass explicit :option:`--target` and :option:`--RTS` on every command
  line involving project files afterwards.

For example:

.. code-block:: ada

  project Monitor is
    for Target use "powerpc-wrs-vxworks7r2";
    for Runtime ("Ada") use "rtp";

    for Object_Dir use "obj-" & Project'Runtime("Ada");
    for Main use ("monitor.adb");
  end Monitor;

We can now instrument with::

  gnatcov instrument -Pmonitor.gpr --level=stmt+decision
    --dump-trigger=main-end --dump-channel=base64-stdout

This is VxWorks where we don't necessarily have an ``atexit`` service. Our
program doesn't have tasks so ``main-end`` is a suitable alternative. The
*stmt+decision* instrumentation will let us assess either *statement* coverage
alone or *statement* and *decision* coverage afterwards.

Building the instrumented version of the program is then achieved with::

  gprbuild -p -Pmonitor.gpr
    --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts.gpr

Execute, extract a trace and report
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The steps required to execute are very environment specific. Symbolically,
we do something like::

  run-cross obj-rtp/monitor.vxe > monitor.stdout

In our case, we have stubbed 4 sensors and obtain an output such as::

  Sensor( 1) =  1 !Ok!
  Sensor( 2) =  5 !Ok!
  Sensor( 3) =  3 !Ok!
  Sensor( 4) =  7 !Ok!

  == GNATcoverage source trace file ==
  R05BVGNvdiBzb3VyY2UgdHJhY2UgZmlsZQAAAAAAAAAAAAAABAEAAAAAAAEAAAAHbW9
  uaXRvcgAAAAACAAAACAAAAAAAAAAAAAAAAwAAAAAAAAAAAAAAAAAAAAcAAAAHAAAAAg
  AAAAAAALNVLgQbmnY19sbrMoReNvzLLN1DAABtb25pdG9yAF8
  == End ==

From which we can extract a source trace file like so::

  gnatcov extract-base64-trace monitor.stdout mon.srctrace

And finally produce a report, with a |gcvcov| command such as::

  gnatcov coverage --level=stmt+decision --annotate=xcov mon.srctrace -Pmonitor.gpr

Library instrumented separately, native configuration, trace output
-------------------------------------------------------------------

For this case we will consider a sample native software system with two source
directories: one ``code`` directory with the sources of a library to test, and a
``tests`` directory with main programs verifying that the library services and
operate as intended.

For the sake of the example, we will consider that

- The library source code is not going to change, and

- We will be adding tests and assess the achieved coverage
  by each new test individually or for the current set of tests
  at a given point in time.

Setting up the coverage runtime
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

On a native system such as Linux or Windows, the simplest is to pick a
*gnatcov_rts_full.gpr* variant, thanks to which we will be able to produce
trace files directly. We go for a straightforward setup assuming we will use
the default full Ada runtime (no specific :option:`--RTS` option)::

  # Copy the sources into a fresh local place for the build:
  cp -rp <gnatcoverage-install>/share/gnatcoverage/gnatcov_rts <gnatcov_rts-build-dir>

  # Build and install the library to a place of our choice.
  cd <gnatcov_rts-build-dir>
  gprbuild -Pgnatcov_rts_full.gpr -f -p

  rm -rf <gnatcov_rts-install-dir>
  gprinstall -Pgnatcov_rts_full.gpr -p --prefix=<gnatcov_rts-install-dir>

  # Allow references to the coverage runtime project from other project files:
  export GPR_PROJECT_PATH=<gnatcov_rts-install-dir>/share/gpr

Project file architecture
^^^^^^^^^^^^^^^^^^^^^^^^^

A possible straightforward way to handle code + tests system when all the code
is available upfront is to setup a single project file designating the two
source dirs and the main units within the *tests* component.

When part of the code, as the set of tests in our case, is being developed and
the other is frozen, best is to isolate the frozen part as a separate project
and declare it ``Externally_Built`` once the instrumented version has been built.

This would normally be achieved by :command:`gprinstall` after the build,
except the support for instrumentation artifacts (:option:`--src-subdirs`
option) may not be available.

One solution consists in setting up a separate library project file for the
library ``code`` part, build the library, use the build tree in-place as the
installation prefix, and switch the ``Externally_Built`` attribute to ``"True"``
before proceeding with separate steps for the tests, instrumenting main units
in particular.

Using an scenario variable to influence the ``Externally_Built`` status, we could
have something like the following project file for the library:

.. code-block:: ada

  --  code.gpr
  library project Code is

    for Library_Name use "code";
    for Library_Kind use "static";
    for Library_Dir use "lib-" & Project'Name;

    for Object_Dir use "obj-" & Project'Name;

    for Source_Dirs use ("code");

    type Mode is ("build", "instrument", "use");
    LIB_MODE : Mode := external ("CODE_LIBMODE", "use");

    case LIB_MODE is
       when "build"      => for Externally_Built use "False";
       when "instrument" => for Externally_Built use "False";
       when "use"        => for Externally_Built use "True";
    end case;

  end Code;

And for the tests, a separate project file where we can list
the main units and state that none of the test units are of interest
to the coverage metrics:

.. code-block:: ada

  --  tests.gpr
  with "code.gpr";

  project Tests is
    for Source_Dirs use ("tests");
    for Object_Dir use "obj-" & Project'Name;

    for Main use ("test_inc.adb");

    package Coverage is
      for Units use ();
    end Coverage;
  end Tests;

Instrument and build the library
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We would first instrument and build the instrumented library with commands
such as::

  gnatcov instrument -Pcode.gpr -XCODE_LIBMODE=instrument --level=stmt+decision

  gprbuild -f -Pcode.gpr -XCODE_LIBMODE=build -p
    --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts_full.gpr

Both commands proceed with ``Externally_Built`` ``"False"``. There is no main unit
attached to the library per se, so no need for :option:`--dump-trigger` or
:option:`--dump-channel` at instrumentation time.

Then we can go on with the tests using the default ``CODE_LIBMODE`` value,
implicitly switching the attribute to ``"True"``.

Instrument, build and run the tests to produce traces
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Here the only point of the instrumentation phase is to instrument the main
units, in our case to dump trace files when the test programs exit::

  gnatcov instrument -Ptests.gpr --level=stmt+decision
    --dump-trigger=atexit [--dump-method=bin-file] --externally-built-projects

The :option:`--externally-built-projects` option is required to consider units
from the library code project as contributing to the set of units of interest,
for the purpose of instrumenting mains, that is, so the instrumentation of
main considers coverage data from those units when producing the trace file.

The build of instrumented tests then proceeds as follows::

  gprbuild -Ptests.gpr -p
    --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts_full.gpr

And a regular execution in the host environment would produce a source
trace in addition to performing the original functional operations.
