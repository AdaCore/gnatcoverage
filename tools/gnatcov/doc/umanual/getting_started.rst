***************
Getting Started
***************

|gcp| provides coverage analysis facilities through the |gcv| command-line
tool. |gcv| relies on an instrumented execution environment to produce
execution traces instead of having to instrument to program itself. |gem|
provides such an environment, offering support for coverage assessments
directly on the target code in cross configurations.

|gcp| supports both source and object level coverage criteria: statement,
decision, or mcdc coverage for the source level, and instruction or branch
coverage for the object level. Once your application is built, a typical
analysis proceeds in two steps:

1) Use |gcvrun| to run your application within the instrumented environment,
   producing <yourapp.trace>. gnatcov determines the target architecture
   automatically from the executable headers::

     gnatcov run <yourapp>

2) Use |gcvcov| to produce a coverage report from the execution trace, with a
   command line that looks like the following::

     gnatcov coverage --level=<criterion> --annotate=<report-format>
        [--scos=@<ALIs>] [--routines=@<symbols>] <yourapp.trace>

Very briefly here:

- :option:`--level` specfies the coverage criterion to be assessed
  (:option:`=stmt`, :option:`=stmt+decision`, or :option:`=stmt+mcdc` for
  source levels; :option:`=insn` or :option:`=branch` for object levels)

- :option:`--annotate` specifies the desired output report format
  (synthetic list of coverage violations, text or html annotated sources, ...)

- :option:`--scos` is specific to the source level criteria, and mandatory in
  this case. This conveys the so called `Source Coverage Obligations` (SCOs),
  which help drive the assessment process and at the same time specify the set
  of source units for which a report should be produced.  The argument value
  in the example here (using the @ notation) is the name of a file which
  contains the set of Ada ALI files or C GLI files corresponding to the source
  units of interest.

- :option:`--routines` is specific to the object level criteria, and optional
  in this case. This conveys the set of object symbol names on which the analysis
  should focus, if any.


For source coverage assessments, sources must be compiled with
:option:`-g -fpreserve-control-flow -fdump-scos`.
Optimization is supported up to :option:`-O1` and inlining is allowed.
For backwards compatibility, for Ada :option:`-gnateS` can be used as
a synonym of :option:`-fdump-scos`.

Object coverage analysis proceeds in a similar fashion, with different
:option:`--level` option values. There is no `source` coverage obligation
involved (by definition of *object* coverage), so no :option:`--scos`
argument, and no specific constraint on the compilation options.

Beyond the simple cases sketched above, |gcp| supports advanced capabilities
available for both source and object coverage criteria. Two examples are
*coverage consolidation*, computing results for a set of execution traces, and
*exemption regions*, allowing users to define code regions for which coverage
violations are expected and legitimate.

The following chapters provide many more details on the various possible modes
of operation. Prior to this, next in this chapter, comes a complete example
sequence illustrating steps from compilation to coverage analysis of a very
simple Ada program.


**Example**

We now provide an example of complete |gcp| use sequence, from source
compilation to coverage analysis. We start from the very basic Ada package
below, with a spec and body in source files named ``ops.ads`` and ``ops.adb``:

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

We analyse the coverage achieved by the sample unit :term:`test driver` below,
in ``test_inc.adb``:

.. code-block:: ada

   with Ops;
   procedure Test_Inc is
     X : Integer := 4;
   begin
     Ops.Apply (Ops.Increment, X);
     pragma Assert (X = 5);
   end Test_Inc;

We use the GNAT Pro toolset for powerpc-elf to build, and GNATemulator for the
same target (invoked by gnatcov run) to emulate. The executable construction
is performed using :command:`gprbuild`, operating on the following ``ops.gpr``
project file::

   project Ops is
    for Languages use ("Ada");
    for Source_Dirs use ("src");
    for Object_Dir use "obj";
   end Ops;

First, we build with this command line::

   gprbuild -p --target=powerpc-elf --RTS=powerpc-elf/zfp-prep -Pops.gpr test_inc.adb
    -cargs:Ada -fdump-scos -gnata -cargs -g -fpreserve-control-flow

In this particular case:

- :option:`-p` queries the creation of the "obj" object directory if it
  doesn't exist. This is where the object, ALI, and executable files will
  reside.

- :option:`--target` and :option:`--RTS` tell gprbuild which target toolchain
  and runtime library to use. Here, powerpc-elf and a zero-footprint library
  tailored for the ``prep`` GNATemulator board.

- :option:`-Pops.gpr` :option:`test_inc.adb` designate the project file and
  the main unit to build.

- :option:`-cargs:Ada` sets the Ada specific compilation option and
  :option:`-cargs` sets the more general ones in accordance with the
  guidelines stated earlier.

The build command produces a ``test_inc`` executable in the object
subdirectory. Our second step is to execute this program within the
instrumented execution environment, via GNATemulator, to obtain a
``test_inc.trace`` execution trace. We do this with |gcvrun|, as follows::

  gnatcov run obj/test_inc

Now, we can analyse the coverage achieved by this execution using
|gcvcov|, for example with the following command line::

  gnatcov coverage --level=stmt --annotate=xcov test_inc.trace --scos=obj/ops.ali

Here, we request

- a source *statement coverage* assessment with :option:`--level=stmt`

- an annotated source report in text format  with :option:`--annotate=xcov`

- focus on the functional unit only with :option:`--annotate=obj/ops.ali`

This produces a single annotated source in the current directory,
``ops.adb.xcov`` quoted below:

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
