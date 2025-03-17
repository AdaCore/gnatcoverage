.. _src_traces:

#####################################
Producing source traces with |gcvins|
#####################################

The production of :term:`source traces <Source Trace>` is performed by an
instrumented version of the original program running in its regular execution
environment. A |gcvins| command produces alternative sources for units of
interest, with additional data structures and code statements solely aimed at
tracking coverage related information. Coverage data is output in accordance
with a user selectable policy, for example upon exit of the program, for which
the program main unit gets instrumented as well.

To gather coverage data while the program is running and to output this data in
the form of a source trace file eventually, the instrumented code relies on
common data types and subprograms provided by a :term:`coverage runtime
<Coverage Runtime>` library.  The first thing to do for a given project is then
to build and install the coverage runtime so it becomes available to the
instrumented sources afterwards, as we will describe in the :ref:`instr-rts`
section of this chapter.

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

Setting up the :term:`coverage runtime <Coverage Runtime>` library
==================================================================

|gcp| is shipped with the sources of the coverage runtime and with a companion
project file. The |gcvstp| command helps automating the build and installation
of this project.

Just like :command:`gprbuild`, |gcvstp| accept the :cmd-option:`--RTS`,
:cmd-option:`--config` and :cmd-option:`--target` command line options: you
need to build the coverage runtime with the same toolchain and runtime as the
ones used to build the application code.  For instance:

.. code-block:: sh

   # For a native project
   gnatcov setup

   # For a project that targets ARM ELF with the light-stm32f4 runtime
   gnatcov setup --target=arm-eabi --RTS=light-stm32f4

By default, |gcvstp| installs the coverage runtime in the same prefix as the
selected toolchain (just like :command:`gprinstall`). In order to install it in
another location, pass the :cmd-option:`--prefix` option:

.. code-block:: sh

   gnatcov setup --prefix=/usr/custom

When installing the coverage runtime to a non-default location, tools that
process project files must be instructed how to find the coverage runtime
project file that is installed.  To achieve this, update the
``GPR_PROJECT_PATH`` environment variable:

.. code-block:: sh

   export GPR_PROJECT_PATH=$GPR_PROJECT_PATH:/usr/custom/share/gpr

In addition, when using shared libraries, the environment needs to be updated
so that instrumented programs can load the coverage runtime's own shared
libraries. The following command achieves this in a Unix like environment:

.. code-block:: sh

   export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/path/to/gnatcov-rts/lib

The following command performs the corresponding action on Windows systems:

.. code-block:: bat

   set PATH=%PATH%;C:\path\to\gnatcov-rts\bin\

Multiple runtimes in the same prefix
------------------------------------

It is sometimes convenient to install multiple times the coverage runtime in
the same prefix, for instance when working on a project that runs both on the
native platform and on an embedded target while the two toolchains are also
installed in the same prefix. It is possible to make |gcvstp| install the
coverage runtime under different project names: the default is ``gnatcov_rts``,
and using the :cmd-option:`--install-name` option changes it.

.. code-block:: sh

   # Install the coverage runtime both for native projects (gnatcov_rts_native)
   # and for ARM ELF/light-stm32f4 projects (gnatcov_rts_stm32f4).
   gnatcov setup --install-name=gnatcov_rts_native
   gnatcov setup --target=arm-eabi --RTS=light-stm32f4 \
     --install-name=gnatcov_rts_stm32f4

When building instrumented projects later with :command:`gprbuild`, you then
have to select the appropriate coverage runtime with the
:cmd-option:`--implicit-with` switch:

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
coverage runtime without its Ada units. The
:cmd-option:`--restricted-to-languages` option allows that:

.. code-block:: sh

   # Build and install the coverage runtime with C units only
   gnatcov setup --restricted-to-languages C

   # Build and install the coverage runtime with both Ada and C units (the
   # default).
   gnatcov setup --restricted-to-languages Ada,C

Note that for now, only Ada and C are supported. The core runtime is
implemented in C, so the C language must be enabled in all cases.


Default source trace dump options
---------------------------------

Source trace dump settings (``--dump-trigger``, ``--dump-channel`` and other
related ``--dump-*`` command line options) are used to make |gcvins| produce
instrumented mains that automatically dump source trace files at the end of the
program execution, or to leave this to manually written code (see the full
documentation in :ref:`later sections <gcvins-cmd-line>`).

Without any ``--dump-*`` option, |gcvstp| selects defaults for the target
platform, which |gcvins| can then reuse automatically. There are two ways to
override these defaults: pass ``--dump-*`` options directly to |gcvins|, or to
|gcvstp|. If ``--dump-*`` options are passed to both |gcvstp| then to |gcvins|,
the options passed to the latter take precedence. For instance:

.. code-block:: sh

   # Use what gnatcov thinks the most sensible defaults for the target platform
   # are.
   gnatcov setup
   gnatcov instrument # ...

   # Setup to use atexit/bin-file by default, then use these defaults for
   # instrumentation.
   gnatcov setup --dump-trigger=atexit --dump-channel=bin-file
   gnatcov instrument # ...

   # Make atexit/bin-file the default at setup time, but actually use
   # main-end/base64-stdout for source instrumentation.
   gnatcov setup --dump-trigger=atexit \
                 --dump-channel=bin-file
   gnatcov instrument --dump-trigger=main-end \
                      --dump-channel=base64-stdout # ...

   # Make atexit/bin-file the default at setup time, but actually use
   # main-end for source instrumentation (thus still using the default
   # bin-file).
   gnatcov setup --dump-trigger=atexit \
                 --dump-channel=bin-file
   gnatcov instrument --dump-trigger=main-end # ...

Note that the defaults that |gcvstp| uses for each target platform may change
between versions of |gcp|.

Coverage runtime setup for configurations with no Ada runtime
-------------------------------------------------------------

If the application is not linked against the Ada runtime library (but contains
Ada code), the coverage runtime needs to be setup accordingly, so as not to
rely on any features from the Ada runtime.

This can be achieved with the :cmd-option:`--no-stdlib` |gcvstp| command line
switch. The resulting coverage runtime only supports a ``manual`` or
``main-end`` dump trigger, and will use a special implementation of the
``base64-stdout`` dump channel, relying on a user-provided function to output
the coverage data.

The output function expected by the coverage runtime should have the same
signature as the stdlib's ``putchar`` function:

.. code-block:: C

  extern int gnatcov_rts_putchar(int __c);

Note that the coverage runtime will ignore the return value.

In all cases the coverage runtime still requires an implementation of
``memset`` and ``memcpy`` to be provided, either from the C standard library,
or provided by the user.

.. note::
  This method of providing a function name upon which the coverage library
  depends, without adding this information in the project file means that
  gprbuild will produce a link command with the assumption that the coverage
  runtime does not depend on the instrumented project. This may result in
  the executable link failing due to an undefined reference to the output
  function symbol name, if the object file is not already pulled in the
  executable in a non-instrumented build.

  To work around this, either ensure the output function is defined in a
  compilation unit that is part of the executable in a non-coverage build,
  or use an extending project to add your own sources to provide the
  ``gnatcov_rts_putchar`` symbol, as detailed in the
  :ref:`section dedicated to coverage runtime customization<basic_rts_custom>`.

Instrumenting programs
======================

Instrumentation is performed upfront for an intended strictest coverage
criterion on a given set of :term:`units of interest <Units of Interest>`. The
production of a coverage report afterwards might restrict the report to a
subset of those units, or lower to a less strict criterion.  Instrumenting
programs is achieved with |gcvins| commands, which might involve two distinct
kinds of operations:

- Modify the code in units-of-interest so the program records, while it is
  running, facts of relevance to the coverage metrics to be assessed,

- Modify the main unit(s) to output the so gathered coverage data to
  an externally readable channel, typically either a source trace file or some
  communication port.

.. _gcvins-cmd-line:

|gcvins| command line
---------------------

As for other commands, help on the command line interface is displayed
by ``gnatcov instrument --help``. The general synopsis is as follows::

  gnatcov instrument --level=<> <units-of-interest> [OPTIONS]

:cmd-option:`--level` states the strictest criterion that this instrumentation
will allow assessing afterwards and the ``<units-of-interest>`` switches
specify the set of units for which such assessment will be possible.  The
latter may only resort to project file facilities, as described in the
:ref:`passing_gpr` section of this manual. Projects marked ``Externally_Built``
in the closure are not instrumented or otherwise modified.

A few :cmd-option:`[OPTIONS]` allow controlling the instrumentation of main
units, if any are designated by the root project:

:cmd-option:`--dump-trigger`
   selects the execution point at which the output of coverage data should be
   injected in main units. The default is inferred from the installed
   instrumentation runtime. The possible choices are ``manual``, ``atexit``,
   ``main-end`` and ``ravenscar-task-termination``.

:cmd-option:`--dump-channel`
   selects the mechanism used to output coverage data at the selected
   triggering point, if any. The possible choices are ``bin-file``, to create a
   source trace file, or ``base64-stdout`` to emit a base64 encoded version of
   the data through ``GNAT.IO``. ``bin-file`` is the default.

:cmd-option:`--externally-built-projects`
   instructs the instrumenter to look into projects marked as externally built
   when computing the list of units of interest (they are ignored by default),
   for the sole purpose of instrumenting mains.

In addition, for trace files produced automatically from a ``bin-file``
dump-channel, the ``--dump-filename-<>`` family of switches provides control
over the name of trace files. See :ref:`instr-tracename` for more details on
the default behavior and possibilities to alter it.

The instrumentation process can be parallelized using the ``-j`` (shorthand for
``--jobs``) switch. ``-j0`` can be used for maximal parallelism, and ``-jN``
with N > 0 to specify the desired level of concurrency.

.. _Dump_Triggers:

Output strategies for main units
--------------------------------

The choice of a ``--dump-trigger``/``--dump-channel`` pair for main units
depends on the runtime environment available to your program.

For a native program with access to a full Ada runtime and the associated
coverage runtime, ``--dump-channel=bin-file`` is the recommended choice as it
produces a trace in the most direct manner and separates the trace data from
the regular output. ``--dump-trigger=atexit`` is a natural triggering choice in
this case, as it takes care of outputting the data automatically at a point
where we know the program is not going to execute more, regardless of how or
why the program exits.

The ``--dump-trigger=main-end`` alternative simply inserts the calls at the end
of the main subprogram bodies, which may be bypassed if the program exits
abruptly, or miss data if the program has tasks not terminated when execution
of the main subprogram/thread reaches its end.

For Ravenscar programs, another option is to use
``--dump-trigger=ravenscar-task-termination``, which triggers the creation of a
source trace whenever an Ada task terminates.

For more restricted environments where, say, there is limited file IO available
to the program, a ``--dump-channel=base64-stdout`` kind of output is needed in
association with the restricted coverage runtime.

.. _manual_dump:

If none of the available automatic triggering option work out well, full
control is offered by the ``--dump-trigger=manual`` policy where the
instrumenter doesn't actually add any code to main units for emitting the
collected coverage data. You will have to indicate the point at which you wish
to emit this data by inserting:

- a ``pragma Annotate (Xcov, Dump_Buffers);`` pragma statement in Ada code;
- a ``/* GNATCOV_DUMP_BUFFERS */`` comment on its own line in C and C++ code

where necessary in your code. During instrumentation, |gcv| will replace them
with a call to the procedure responsible for dumping the coverage buffers, at
which point the source traces will be created during the execution of the
program. Therefore, the pragma or comment should be placed at a location at
which such a function call would be appropriate. By default, gnatcov will look
into all the files of the project. Alternatively, the user can specify the list
of files containing manual indications using
``--dump-trigger=manual,@FILES.list`` or ``--dump-trigger=manual,FILE1,FILE2``
where the files are specified as full / relative names. Note that for C / C++
files, the user shall specify the files where the ``/* GNATCOV_DUMP_BUFFERS */``
indication is ultimately expanded by the preprocessor.

A dump procedure is only able to dump the buffers of the project tree which
root is the project it is called from. A dump procedure call done in a
subproject will result in a trace containing all code of projects higher in the
project tree marked as not covered.

One source trace is dumped per call to the dump buffers procedure. For the
``bin-file`` dump configuration, each trace is written in a file which name
depends on the selected trace file naming scheme. For the ``base64-stdout``
dump configuration the traces are sequentially dumped in the same output file,
from which the |gcv| command ``extrace-base64-trace`` will be able to produce a
source trace file.

It is also possible to configure the prefix of the trace filename
(see :ref:`instr-tracename` for more details) through these indications:

- ``pragma Annotate (Xcov, Dump_Buffers, Prefix);`` in Ada code, where
  ``Prefix`` resolves to type ``String``;
- ``/* GNATCOV_DUMP_BUFFERS (Prefix) */`` in C or C++ code, where ``Prefix``
  resolves to a null-terminated ``char *`` type.

No type checking will be done during instrumentation, the values passed as
prefix indications are used as parameters to the buffer dump procedure call.
The Ada procedure call expects a ``String`` parameter, and the C or C++
functions expect a null-terminated ``const char *``, so in both cases
specifying a string literal is allowed. If the prefix indication does not
resolve to the correct type, the instrumented code will fail to build.

This can be used in case multiple traces are dumped in rapid succession, to
distinguish between each trace:

.. code-block:: Ada

  procedure Dump is
    Prefix : constant String := "variable";
  begin
    --  Ok, Prefix is of type String
    --  Trace filename will be "variable-<hex_part>.srctrace"

    pragma Annotate (Xcov, Dump_Buffers, Prefix);

    --  Ok, String literal
    --  Trace filename will be "literal-<hex_part>.srctrace"

    pragma Annotate (Xcov, Dump_Buffers, "literal");

    --  Not ok, Undefined does not resolve to a variable of type String
    --  This will fail to compile.

    pragma Annotate (Xcov, Dump_Buffers, Undefined);
  end Dump;

.. code-block:: c

  void dump(void){

    /* Ok, prefix if type char * and null-terminated.
       Trace filename will be "variable-<hex_part>.srctrace"  */
    char *prefix = "variable";
    /* GNATCOV_DUMP_BUFFERS (prefix) */

    /* Ok, string literal.
       Trace filename will be "literal-<hex_part>.srctrace"  */
    /* GNATCOV_DUMP_BUFFERS ("literal") */

    /* Not ok, Undefined does not resolve to a null terminated string.
       This will not compile.  */
    /* GNATCOV_DUMP_BUFFERS (Undefined) */
  }

.. _buff_reset:

Resetting coverage information during the execution
---------------------------------------------------

|gcvins| offers the possibility to indicate, in the program sources, one or
multiple locations at which the buffers used to track the execution of the
program should be cleared. This can be used in various situations where a finer
grain control of the traces generated by the instrumented program is required.
For instance, this can be used to ignore some initialization phase in the
coverage report, or to emit multiple coverage traces from a single execution of
the instrumented program, each trace containing the coverage information for a
specific part of the execution, such as in a unit test driver.

This fine grain control of the coverage buffers mandates the use of a manual
dump of the coverage buffers, thus requiring passing the
``--dump-trigger=manual`` command line option to |gcvins|
invocations, as described in :ref:`Dump_Triggers`.

The buffer reset location indications follow a similar convention as the manual
dump of the traces. Indicating the point at which coverage buffers must be
cleared is done by inserting:

- a ``pragma Annotate (Xcov, Reset_Buffers);`` pragma statement in Ada code;
- a ``/* GNATCOV_RESET_BUFFERS */`` comment on its own line in C and C++ code

where necessary. During instrumentation, |gcv| will replace them
with a call to the procedure responsible for clearing the coverage buffers.
Therefore, the pragmas or comments should be placed at locations at which such
a procedure call would be legal and appropriate.

For performance reasons, no synchronization mechanisms are used to protect the
buffer clear procedure call against concurrent writes in the coverage buffers
(by regular program execution). **It is up to the user to ensure that no other
section of the instrumented code runs concurrently to the buffer clear
procedure, otherwise the correctness of the corresponding coverage report
cannot be guaranteed.**

Both buffer dump indications and buffer reset indications can be specified in
external annotation files, see :ref:`ext_annot` for more information.

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

* The :cmd-option:`--dump-filename-prefix` switch to |gcvins| requests
  replacing the ``<ename>`` component by the switch argument;

* The :cmd-option:`--dump-filename-simple` switch requests the removal of the
  variable components (stamps and pid), so only the ``<ename>`` component
  remains or the replacement provided by :cmd-option:`--dump-filename-prefix`
  if that switch is also used.


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
tailored by passing a :cmd-option:`--dump-filename-env-var` switch to |gcvins|,
providing the variable name to use.

Support for preprocessing
-------------------------

|gcvins| automatically detects preprocessor configuration from the compiler
switches present in project files (``-gnatep`` and ``-gnateD`` for Ada sources,
``-D`` and the like for C/C++ sources).  It then runs preprocessing on the
source code *before* the instrumentation itself happens.  This allows gnatcov
to compute the code coverage only for code that is left "enabled" by
preprocessing directives: disabled code (for instance what follows ``#if Debug
then`` in Ada when the preprocessing symbol ``Debug`` is set to ``False``) is
ignored and thus creates no coverage obligation. Note that consolidation will
not help including code from all "preprocessing branches" in coverage reports,
as gnatcov requires (and checks) that coverage obligations are the same for two
units to consolidate.

Ada pecularities
^^^^^^^^^^^^^^^^

The coverage obligations for code that comes from symbol expansion (for
example, ``$Foo = 42`` expanded into ``My_Variable = 42`` with
``-Dfoo=My_Variable``) designate expanded code.  Even though line numbers are
preserved during preprocessing, column numbers may be different between the
original code and the preprocessed code and thus the coverage report.


.. _instr-limitations:

|gcvins| limitations
--------------------

There are situations and code patterns not handled correctly by |gcvins|.
Below are listed the limitations associated with general Ada sources.
Coverage of SPARK sources require additional considerations, detailed in
section :ref:`spark_instr`.

Unsupported source constructs
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There are a few language constructs that |gcvins| doesn't support.
The tool emits a warning when it encounters such cases and the corresponding
code is not instrumented. Source coverage obligations are still emitted, and
the unsupported constructs will be reported in a separate
``Undetermined_Coverage`` category, to differentiate them from actual coverage
violations.

The list of unsupported constructs is as follows:

* Generic null procedures,
* Protected bodies entry guards when the ``Simple_Barriers`` restriction or
  the ``Pure_Barriers`` one apply.

Additionally, if the Ada language version in use, indicated to the tool by
either a ``pragma Ada_nnnn`` pragma in the sources or through the ``--ada``
command line switch, is less or equal to Ada 2012, the following constructs are
also unsupported:

* Generic expression functions,
* Recursive expression functions which are primitives of some tagged type,
* Expression functions which are primitives of their return type, when it is a
  tagged type.

The simplest way to work around the limitation concerning expression functions
is to turn them into regular functions, by giving them a proper body,
containing a single return statement with the original expression.
Otherwise it is possible to exempt those constructs (see :ref:`exemptions`)
and/or perform a manual coverage analysis for these special cases.

The MC/DC instrumentation of decisions with many conditions may require more
memory than available (during instrumentation and/or at run-time) to enumerate
the possible paths through the decision. To avoid this, |gcv| will not
instrument such decisions for MC/DC, emitting a warning in the process, and the
MC/DC coverage for each decision will be reported as ``Undetermined_Coverage``
state. Should the default limit not be satisfactory, it can be tuned with the
option :cmd-option:`--path-count-limit`.

Other source-traces limitations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In Ada, variable or type declarations at the package level can yield elaboration
code. Such code constructs are thus considered to have corresponding coverage
obligations.

In the case where a ``pragma Preelaborate`` restriction affects the
instrumented unit, variable and type declarations at the package level are not
considered as coverage obligations, although some elaboration code may still be
emitted in rare instances. Note that declarations within a unit constrained by
a ``No_Elaboration_Code`` pragma don't produce coverage obligation either,
which is always correct as no executable code can be emitted by the compiler
for them.

There are also a few limitations concerning the source trace workflow as a
whole:

- Separate analysis of generic package instances is not supported.

Toolchain-specific limitations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

With GNAT versions from 7.1 to 7.3, compiling with optimization will result in
coverage violations on all statement obligations associated with expression
functions. Explicitly disabling optimization (with ``-O0`` for instance) will
resolve this issue.

C/C++ limitations
^^^^^^^^^^^^^^^^^

The instrumentation process yields preprocessed versions of the sources. Thus,
it is required to remove any :cmd-option:`-include` switch that is passed to
the compiler invocation, by having a dedicated scenario variable for a coverage
build for instance.

Function and call coverage limitations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Function and call coverage supports all three Ada, C and C++ languages,
with language-specific limitations.

For the Ada language, gnatcov is currently unable to provided coverage
information for:

* Call expressions part of the prefix of a dotted name

The coverage state of these cases will be reported as undetermined.

For the C++ language, gnatcov will currently not instrument nor provide
coverage information for:

* Constructor and Destructor functions

* Constructor and Destructor calls

Guarded expression coverage limitations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Guarded expression coverage is only available for the Ada language.
For implementation reasons, it is only available starting from version 2022 of
the language.

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
object directory.  Giving priority to this subdir when it exists is achieved by
passing a :cmd-option:`--src-subdirs` switch to :command:`gprbuild`, naming
that particular relative subdirectory.

Then :command:`gprbuild` now supports a :cmd-option:`--implicit-with` option
which requests processing every project as if it started with a ``with``
statement for a given project, which we can use to designate the coverage
runtime project file so all the compiled code gets access to the support
packages.

The build of instrumented components then proceeds almost exactly as a regular
one, only adding :cmd-option:`--src-subdirs=gnatcov-instr` and
:cmd-option:`--implicit-with=<gnatcov_rts_gpr>` to the build options, where
:cmd-option:`<gnatcov_rts_gpr>` would be the coverage runtime project file
setup beforehand for the project, as described previously in this chapter. This
project file could be referred to with a full path specification, or with a
simple basename if the ``GPR_PROJECT_PATH`` environment variable is updated to
designate the directory where the project file is located, which would be the
``share/gpr`` subdirectory of the runtime installation tree.

While the scheme relies on the use of GNAT project files, it does not
absolutely require :command:`gprbuild` to build the instrumented programs,
even though we have augmented that builder with a few features to make that
process very efficient and straightforward.

.. note::

   The variety of supported compilers/compiler versions/optional warnings makes
   it an irrealistic goal for |gcvins| (and source code generators in general) to
   produce warning-free code. For instance, a hypothetical compiler is
   perfectly allowed to warn when functions have more than X statements; yet it
   would not be reasonable for |gcvins| to accomodate this restriction.

   It is important to note that these are warnings (notes that the compiler can
   emit on code deemed suspicious, but which do not abort compilation), as
   opposed to errors, which do abort compilation.

   For these reasons, the most reasonable thing to do with automatically
   generated code is to disable “warnings-as-error” (``-gnatwn`` for GNAT,
   ``-Wno-error`` for GCC) when building instrumented code.

Extracting a trace from standard output
=======================================

With the :cmd-option:`base64-stdout` channel, coverage data is emitted with
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

The |gcp| instrumentation mode supports Ada, C and C++ (beta). Ada and C units
are instrumented by default, however since C++ support is still under
development, it is disabled by default.

To change the set of languages to be instrumented, pass the
:cmd-option:`--restricted-to-languages` option to |gcvins|. For instance, to
instrument only Ada units:

.. code-block:: sh

   gnatcov instrument --restricted-to-languages=Ada # ...

And to instrument Ada, C and C++ units:

.. code-block:: sh

   gnatcov instrument --restricted-to-languages=Ada,C,C++ # ...

.. _spark_instr :

Instrumentation and coverage of SPARK code
==========================================

The instrumentation of a SPARK project requires an additional step in order
to make the compiler accept the instrumented code. Additionally, some parts of
SPARK sources are not processed by the instrumenter, and thus will not have
any coverage obligation attached to them.

Inhibiting SPARK related pragmas
---------------------------------

SPARK introduces a certain number of pragmas and aspects to aid the developer
in writing program contracts and guiding the automatic provers. These are only
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

or by way of a ``Global_Configuration_Pragmas`` project file attribute,
possibly controlled by a scenario variable as in:

.. code-block:: ada

  type mode is ("prod", "coverage");
  BUILD_MODE : mode := external ("BUILD_MODE", "prod")

  package Builder is
    case BUILD_MODE is
      when "coverage" => for Global_Configuration_Pragmas use "instrument-spark.adc";
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
coverage obligation attached to them in the coverage reports, unless
explicitly requested by enabling the coverage of assertions.

The concerned pieces of code are notably:

- any entity that is ``Ghost``
- any contract (``Pre``/``Post``/``Contract_Cases``/``Loop_Invariant``)

Note that since no coverage obligations are emitted for such source
constructs, they will not appear in the coverage reports even if assertions
are enabled and the assertion policy enables the compilation of ghost code.

It is however possible to request coverage information for some contracts that
generate executable code with assertion coverage levels, as described in
:ref:`scov-atc` and :ref:`scov-atcc`. Note that any ghost code that is not
part of a contract will never be instrumented.

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
Time Processes hence an :cmd-option:`rtp` Ada runtime library. We will assume
we don't have a filesystem at hand, so will rely on the base64 encoded output
of trace data to standard output.


Setting up the coverage runtime
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As seen in the :ref:`instr-rts` section, we use the ``gnatcov setup`` command to
build and install the :term:`coverage runtime <Coverage Runtime>`.

For our intended target environment, this would be something like::

  gnatcov setup --target=powerpc-wrs-vxworks7r2 --RTS=rtp \
    --prefix=<gnatcov_rts-ppc-install-dir>

  # Allow references to the coverage runtime project from other project files:
  export GPR_PROJECT_PATH=<gnatcov_rts-ppc-install-dir>/share/gpr

Instrument and build
^^^^^^^^^^^^^^^^^^^^

We setup a ``monitor.gpr`` project file for our program, where we

- Provide the main unit name, so it can be instrumented automatically, and...

- State the target configuration name and Ada runtime library so we won't have
  to pass explicit :cmd-option:`--target` and :cmd-option:`--RTS` on every
  command line involving project files afterwards.

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

As seen in the :ref:`instr-rts` section, we use the ``gnatcov setup`` command to
build and install the :term:`coverage runtime <Coverage Runtime>`::

  gnatcov setup --prefix=<gnatcov_rts-install-dir>

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
except the support for instrumentation artifacts (:cmd-option:`--src-subdirs`
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
    --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts.gpr

Both commands proceed with ``Externally_Built`` ``"False"``. There is no main
unit attached to the library per se, so no need for
:cmd-option:`--dump-trigger` or :cmd-option:`--dump-channel` at instrumentation
time.

Then we can go on with the tests using the default ``CODE_LIBMODE`` value,
implicitly switching the attribute to ``"True"``.

Instrument, build and run the tests to produce traces
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Here the only point of the instrumentation phase is to instrument the main
units, in our case to dump trace files when the test programs exit::

  gnatcov instrument -Ptests.gpr --level=stmt+decision
    --dump-trigger=atexit [--dump-method=bin-file] --externally-built-projects

The :cmd-option:`--externally-built-projects` option is required to consider
units from the library code project as contributing to the set of units of
interest, for the purpose of instrumenting mains, that is, so the
instrumentation of main considers coverage data from those units when producing
the trace file.

The build of instrumented tests then proceeds as follows::

  gprbuild -Ptests.gpr -p
    --src-subdirs=gnatcov-instr --implicit-with=gnatcov_rts.gpr

And a regular execution in the host environment would produce a source
trace in addition to performing the original functional operations.


Coverage runtime customization
==============================

.. _basic_rts_custom:

Basics
------

Some configurations have constraints that make the default coverage runtime
inadequate. For instance, targeting a board that has no serial port, making the
default implementation for ``--dump-channel=base64-stdout`` ineffective. In
order to accomodate for such special needs, it is possible to extend the
coverage runtime project and add/override some of its sources.

First, build and install the default coverage runtime (``gnatcov_rts``):

.. code-block:: sh

   # Add --target / --RTS / --no-stdlib if needed according to the toolchain to use
   gnatcov setup

Then create the project extension. The integration with the |gcvstp| workflow
requires the project file and the sources to be in a dedicated directory:

.. code-block:: sh

   mkdir my_rts
   cat > my_rts/my_rts.gpr <<EOF
   project My_RTS extends "gnatcov_rts" is
      for Object_Dir use "obj." & GNATcov_RTS.Library_Type;
      for Library_Dir use "lib." & GNATcov_RTS.Library_Type;
   end My_RTS;
   EOF

It is then possible to add source files to the ``my_rts`` directory to be
included in this custom coverage runtime. Like in non-customized cases,
|gcvstp| can then be used to buid and install the coverage runtime:

.. code-block:: sh

   # Add --target and --RTS if needed according to the toolchain to use
   gnatcov setup my_rts/my_rts.gpr

While it is technically possible with this mechanism to modify all aspects of
the default coverage runtime library, in practice only one use case is
supported currently: changing the behavior of the
``--dump-channel=base64-stdout`` instrumentation option.

.. _custom_base_64:

Customize the ``base64-stdout`` dump channel
--------------------------------------------

To achieve this, the coverage runtime defines a single routine, in charge of
writing bytes to "the output stream":

* When Ada support is enabled, in the ``gnatcov_rts-base_io.adb`` source file,
  using the Ada runtime package ``GNAT.IO`` to write to the serial port
  (bareboard targets) or to the standard output (native targets).

* When Ada support is disabled, in the ``gnatcov_rts-base_io.c`` source file,
  using the libc's ``fwrite`` function on ``stdout``.

Overriding the source file for the relevant configuration is enough to
customize the ``base64-stdout`` dump channel.

When Ada support is enabled, override the ``gnatcov_rts-base_io.adb`` source
file so that it can still be built with the (unmodified)
``gnatcov_rts-base_io.ads`` source file.

.. code-block:: ada

   --  gnatcov_rts-base_io.ads

   with GNATcov_RTS.Strings; use GNATcov_RTS.Strings;

   package GNATcov_RTS.Base_IO is
      pragma Preelaborate;
      procedure Put (S : GNATcov_RTS_String);
      pragma Export (C, Entity => Put, External_Name => "gnatcov_rts_put_string");
   end GNATcov_RTS.Base_IO;

   --  gnatcov_rts-strings.ads

   with Interfaces.C; use Interfaces.C;

   package GNATcov_RTS.Strings is
      pragma Pure;
      type GNATcov_RTS_String is record
         Str    : System.Address;
         Length : size_t;
      end record;
      pragma Convention (C_Pass_By_Copy, GNATcov_RTS_String);
   end GNATcov_RTS.Strings;

When Ada support is disabled, override the ``gnatcov_rts_c-base_io.c`` source
file so that it implements the interface described in the (unmodified)
``gnatcov_rts_c-base_io.h`` source file.

.. code-block:: c

   /* gnatcov_rts_c-base_io.h */

   #include "gnatcov_rts_c_strings.h"
   extern int gnatcov_rts_put_string (struct gnatcov_rts_string str);

   /* gnatcov_rts_c_strings.h */

   #include <stddef.h>
   struct gnatcov_rts_string
   {
     const char *str;
     size_t length;
   };

Building instrumented programs with CCG
=======================================

Programs instrumented with |gcvins| can be built using the
`GNAT Pro Common Code Generator <https://docs.adacore.com/live/wave/gnat-ccg/html/gnatccg_ug/gnat_ccg.html>`_
provided a few modifications are made to the coverage runtime, and
respecting some limitations in terms of dump-trigger and dump-channel choice.

Customizing the runtime
-----------------------

Given the workflow associated with CCG, it is not possible to use the |gcvstp|
command to setup the coverage runtime. Instead, it must be prepared manually.

The coverage runtime contains both Ada and C sources. When using CCG through
GPRbuild, projects containing C sources are not well handled and some steps of
the build procedure won't be executed. There thus is an external variable to
remove all C sources from the project. This means that C sources must be
manually managed during the executable compilation later on.

The first step is to copy the runtime sources in a dedicated directory. For the
remainder of this section, it will be denoted by ``ccg_gnatcov_rts``.

.. code-block:: sh

   mkdir ccg_gnatcov_rts
   cp -r <gnatdas_install_dir>/share/gnatcoverage/gnatcov_rts ccg_gnatcov_rts

Then, C sources not relevant to the coverage runtime for the CCG configuration
must be deleted:

.. code-block:: sh

   rm ccg_gnatcov_rts/gnatcov_rts_c-base-io.c
   rm ccg_gnatcov_rts/gnatcov_rts_c-trace-output-files.*
   rm ccg_gnatcov_rts/gnatcov_rts_c-os_interface.*

Finally, it is necessary to modify the contents of
``ccg_gnatcov_rts/gnatcov_rts-base_io.adb`` to use an alternate medium on which
the execution trace will be output. By default this relies on ``GNAT.IO``,
which is not available in the CCG runtime. It is possible to replace occurrences
of this unit by ``Ada.Text_IO``, which is supported by CCG, but which relies on
the C standard function ``putchar``. Otherwise, see :ref:`custom_base_64` for
more details on the expected interface to dump the coverage trace information.

Building an instrumented program with CCG
-----------------------------------------

Building an instrumented program with CCG is done in the same manner as with
any other target, with the exception that GPRbuild must be instructed to use
the custom coverage runtime prepared in the previous step, and by setting an
external variable to configure the coverage runtime project for a build with
CCG:

.. code-block:: sh

   gprbuild -P <user_project.gpr> --src-subdirs=gnatcov-instr \
     --implicit-with=ccg_gnatcov_rts/gnatcov_rts.gpr \
     -XGNATCOV_RTS_FOR_CCG=true \
     <relevant target cargs>

After the generation of C sources is complete, all the coverage runtime C sources
must be included in the build process of the executable. These can be found
under ``ccg_gnatcov_rts/`` for the sources already present before the gprbuild
invocation, and under ``ccg_gnatcov_rts/obj-gnatcov_rts.static`` for the sources
generated by CCG. The C sources generated from the instrumented Ada sources will
be available in the object directory of their corresponding project.
