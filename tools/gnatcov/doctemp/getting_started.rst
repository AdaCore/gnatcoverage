***************
Getting Started
***************

|gcp| provides the |gcv| command-line tool for coverage analysis operations.
The tool relies on an instrumented execution environment such as |gem| to
produce target machine code execution traces, without any instrumentation of
the program itself.

For source coverage assessments ('statement', 'decision'
or 'mcdc' coverage criterion), sources must be compiled with
:option:`-g -fpreserve-control-flow`, plus :option:`-gnateS` for Ada or
:option:`-fdump-scos` for C. Optimization is supported up to :option:`-O1`
and inlining is allowed.

Once your application is built, the analysis proceeds in two
steps. Essentially:

::

   1) gnatcov run --target=<target> <yourapp.elf>

Runs you application within the instrumented environment suitable for
<target>, producing <yourapp.trace>. Then:

::

   2) gnatcov coverage --level=<criterion> --scos=@<ALIs> 
       --annotate=<report-format> <yourapp.trace>

Produces a coverage report from the execution trace.

The :option:`--scos` option conveys the so called `Source Coverage
Obligations` (SCOs), which drive the assessment process for source
criteria and at the same time specify the set of source units for which a
report should be produced.  The argument value in the example here (using the
@ notation) is the name of a file which contains the set of Ada ALI files or C
GLI files corresponding to the source units of interest.

For example, to obtain (a) a statement coverage report, (b) in html format,
(c) focused on two Ada units u1.adb and u2.adb, (d) from a program
compiled into utest.elf, (e) for a powerpc-elf target, you would do:

::

   1) gnatcov run --target=powerpc-elf utest.elf
                         (e)              (d)

   2) <write a "ualis" file with this contents:
       u1.ali
       u2.ali
      > (c), then:

      gnatcov coverage --level=stmt --annotate=html --scos=@ualis utest.trace
                             (a)           (b)            (c)          (d)

Object coverage analysis (machine 'instruction' or 'branch' coverage
criterion) proceeds in a similar fashion, with different :option:`--level`
option values and no `source` coverage obligation so no :option:`--scos`
argument either.

The following chapters provide further details on the various possible
modes of operation.

