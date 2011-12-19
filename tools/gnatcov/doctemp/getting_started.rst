***************
Getting Started
***************

|gcp| provides the |gcv| command-line tool for coverage analysis operations,
relying on an instrumented execution environment such as |gem| to produce
target machine code execution traces.

For source coverage assessments ('statement', 'decision'
or 'mcdc' coverage criterion), sources must be compiled with
`-g -fpreserve-control-flow`, plus `-gnateS` for Ada or
`-fdump-scos` for C. Optimization is supported up to `-O1`
and inlining allowed.

Once your application is built, the analysis proceeds in two
steps. Essentially:

::

   1) gnatcov run --target=<target> <yourapp.elf>

Runs you application within the instrumented environment suitable for
<target>, producing <yourapp.trace>. Then:

::

   2) gnatcov coverage --level=<criterion> --scos=@<ALIs> 
       --annotate=<report-format> <yourapp.trace>

Produces a coverage report from the execution trace.  The `--scos`
option conveys the so called ``Source Coverage Obligations'', which drive the
assessment process for source criteria and at the same time specify the set of
source units for which a report should be produced.  The argument value in the
example here (using the @ notation) is the name of a file which contains the
set of Ada ALI files or C GLI files corresponding to the source units of
interest.

For example, to obtain a statement coverage report (a) in html format
(b) focused on two Ada units u1.adb and u2.adb (c) from a program
compiled into utest.elf (d) for a powerpc-elf target (e), you would do:


::

    1) gnatcov run --target=powerpc-elf utest.elf
                          (e)              (d)

    2) <write a "ualis" file with this contents:
        u1.ali
        u2.ali
       > (c), then:

       gnatcov coverage --level=stmt --annotate=html --scos=@ualis utest.trace
                              (a)           (b)            (c)          (d)

  

For mcdc analysis, you must pass the --level and --scos options to gnatcov run
as well.

Object coverage analysis (machine instruction or branch coverage)
proceeds in a similar fashion, with different `--level` option
values and no 'source' coverage obligation so no `--scos`
argument either.

The following chapters provide further details on the various possible
modes of operation.

@page

