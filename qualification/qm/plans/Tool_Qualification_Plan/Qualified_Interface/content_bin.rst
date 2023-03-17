.. _qualified-interface:

GNATcoverage Qualified Interface
================================

To obtain reports suitable for use as certification evidence, applicants shall
use GNATcoverage as follows:

* Build the application with the GNAT Pro toolchain identified in the
  *Operational Environment* section of the TOR document, obeying the
  compilation options and coding standard rules documented there as well.

* Build the test code and test harness and link it with the relevant
  application objects if necessary to form one or several test
  executables. The test code does not need to be compiled with the switches
  described in the Operational Environment section, nor does it need to comply
  to the coding standard.

* Obtain as many execution trace files (<TRACEi>) as needed, by
  running the test executables (<APPi>) within the instrumented
  execution environment, as in the following example.

  <LVL> is the coverage criterion level to be assessed and <TARGET> is
  the target architecture. The :literal:`--target` switch is required
  for cross configurations and shouldn't be stated in the native case.

  The <UNITS> part is required for mcdc analysis only and shall
  designate at least the units of <APPi> that will be of interest to
  the analysis step.

  The form these switches should take is described below.

.. code-block:: text

 gnatcov run [--target=<TARGET>] --level=<LVL> <APP1> -o <TRACE1> [<UNITS>]
 ...
 gnatcov run [--target=<TARGET>] --level=<LVL> <APPn> -o <TRACEn> [<UNITS>]


* Optionally, produce a single <REPORT> file (format documented in the
  GNATcoverage User's Guide), consolidating the coverage outcome for the list
  of execution traces stored in the <traces.list> file, and querying results
  for the source units designated by the <UNITS> argument:

.. code-block:: text

 gnatcov coverage --annotate=report --level=<LVL> <UNITS> @<traces.list> -o <REPORT>


In the sample commands above:

* <APPi> is a test executable;
* <LVL> designates the coverage criteria to assess, depending on the software
  level;

  * For level C, statement coverage data is obtained with :literal:`--level=stmt`;

  * For level B, statement *and* decision coverage data is obtained with
    :literal:`--level=stmt+decision`;

  * For level A, statement *and* decision *and* mcdc coverage data is obtained
    with :literal:`--level=stmt+mcdc`.

* <REPORT> is the output file containing the GNATcoverage report;
* <TARGET> identifies the target platform (as in the GNAT Pro toolchain
  prefixes, e.g.  powerpc-elf);
* <TRACEi> is the output file containing the execution trace
* <UNITS> is a specification of the units for which coverage is to be
  assessed (so called *Units Of Interest*), with either

  * A :option:`--scos=@<alis.list>` switch, where <alis.list> is a text
    file containing the list of GNAT Pro ALI file names associated with
    the units of interest; *or*

  * GPR project file facilities, as documented in the tool User's Guide and
    summarized in the |tor_doc| document.

* <traces.list> is a text file containing the list of execution traces to
  operate on.

IO redirections, sometimes necessary for proper operation of the programs
within the execution environment, are allowed.
