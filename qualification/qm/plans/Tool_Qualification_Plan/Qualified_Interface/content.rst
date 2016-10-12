.. _qualified-interface:

GNATcoverage Qualified Interface
================================
To obtain reports suitable for use as certification evidence, applicants shall use GNATcoverage as follows:

* Build the application with the GNAT Pro toolchain identified in the *Operational Environment* section of the TOR document, obeying the compilation options and coding standard rules documented there as well.

* Build the test code and test harness and link it with the relevant application objects if necessary to form one or several test executables. The test code does not need to be compiled with the switches described in the Operational Environment section, nor does it need to comply to the coding standard.

* Obtain as many execution trace files (<TRACE>) as needed, by running the
  test executables (<APPn>) within the instrumented execution environment,
  specifying the target architecture (<TARGET>) *and* the criterion level to
  be assessed (<LVL>), as in the following example.

  IO redirections, sometimes necessary for proper operation of the programs
  within the execution environment, are allowed in these sequences of
  commands.

.. code-block:: text 
 
 gnatcov run --target=<TARGET> --level=<LVL> <APP1> -o <TRACE>
 ...
 gnatcov run --target=<TARGET> --level=<LVL> <APPn> -o <TRACE>


* Produce a single <REPORT> file (format documented in the GNATcoverage User's Guide), consolidating the coverage outcome for the list of execution traces stored in the <traces.list> file, and querying results for the list of source units designated in the <alis.list> file:

.. code-block:: text

 gnatcov coverage --annotate=report --level=<LVL> --scos=@<alis.list> @<traces.list> -o <REPORT>

In the sample commands above:

* <APPn> is a test executable
* <LVL> designates the coverage criteria to assess, depending on the software level.

  * For level C, statement coverage data is obtained with :literal:`--level=stmt`

  * For level B, statement *and* decision coverage data is obtained with
    :literal:`--level=stmt+decision`

  * For level A, statement *and* decision *and* mcdc coverage data is obtained
    with :literal:`--level=stmt+mcdc`

* <REPORT> is the output file containing the GNATcoverage report
* <TARGET> identifies the target platform (as in the GNAT Pro toolchain prefixes, e.g.  powerpc-elf);
* <TRACE> is the output file containing the execution trace
* <alis.list> is a text file containing the list of GNAT Pro ALI file names associated with the units for which coverage is assessed
* <traces.list> is a text file containing the list of execution traces to operate on.
