.. _qualified-interface:

GNATcoverage Qualified Interface
================================

To obtain reports suitable for use as certification evidence, applicants shall
use GNATcoverage as follows:

* Build and install the coverage runtime in a dedicated <GNATCOV_RTS_DIR>
  directory. Use the GNAT Pro toolchain identified in the
  *Operational Environment* section of the |tor_abb| document for this,
  taking care to specify the <TARGET> and <RTS> if the application is
  not a native one.

  This would be achieved with a command such as:

  .. code-block:: text

   gnatcov setup [--target=<TARGET> --RTS=<RTS>] --prefix=<GNATCOV_RTS_DIR>


  This step needs only to be done once, provided that the *Operational
  Environment* described in the |tor_abb| document has not changed as of
  the previous coverage runtime setup.


* Instrument the application for coverage assessment and test harness main
  units for coverage buffer dumping using the ``gnatcov instrument`` command;
  Specify the coverage criterion level to be assessed as  well as a trace
  <DUMP_METHOD> and <DUMP_CHANNEL>  as specified in the
  *Operational Environment* section of the |tor_abb| document. Also specify the
  set of units of interest for which coverage should be assessed trough
  the <UNITS> arguments, which should at least include a <ROOT PROJECT>
  indication using the :literal:`-P<ROOT_PROJECT>` switch:

  .. code-block:: text

   gnatcov instrument [--target=<TARGET> --RTS=<RTS>]
      --dump-trigger=<DUMP_TRIGGER> --dump-channel=<DUMP_CHANNEL>
      --level=<LVL> -P<ROOT_PROJECT> <UNITS>


* Build the application and test executables with the GNAT Pro toolchain
  identified in the *Operational Environment* section of the |tor_abb| document,
  obeying the coding standard rules, and compilation switches documented there
  as well. The gprbuild invocation to build the application shall contain the
  ``--src-subdirs=gnatcov-instr`` argument on the command line, as well as a
  ``--implicit-with=gnatcov_rts.gpr`` argument to link against the coverage
  runtime. The coverage runtime project must be made available to gprbuild by
  adding ``<GNATCOV_RTS_DIR>/share/gpr`` to the ``GPR_PROJECT_PATH`` environment
  variable.


* Obtain as many execution trace files (<TRACEi>) as needed, by
  running the test executables.

* Optionally, produce a single <REPORT> file (format documented in the
  GNATcoverage User's Guide), consolidating the coverage outcome for the list of
  execution traces, produced in the previous step and stored in the
  <traces.list> file, and querying results for the source units designated by
  the <UNITS> argument:

  .. code-block:: text

   gnatcov coverage --annotate=report -o <REPORT>
      --level=<LVL> -P<ROOT_PROJECT> <UNITS> @<traces.list>


In the sample commands above:

* <GNATCOV_RTS_DIR> designates the path to a directory in which the coverage
  runtime will be installed. It shall be accessible for writing and reading by
  the tool;
* <DUMP_TRIGGER> identifies the trigger to produce execution traces, as
  described in the tool User's Manual, and for which the value to be used
  is defined in the |tor_abb| document;
* <DUMP_CHANNEL> identifies the medium used by the instrumented executable to
  output the execution traces, as described in the tool User's Manual and for
  which the value to be used is defined in the |tor_abb| document;
* <LVL> designates the coverage criteria to assess, depending on the software
  level;

  * For level C, statement coverage data is obtained with :literal:`--level=stmt`;

  * For level B, statement *and* decision coverage data is obtained with
    :literal:`--level=stmt+decision`;

  * For level A, statement *and* decision *and* mcdc coverage data is obtained
    with :literal:`--level=stmt+mcdc`.

* <REPORT> is the output file containing the GNATcoverage report;
* <TARGET> identifies the target platform (as in the GNAT Pro toolchain
  prefixes, e.g.  arm-elf);
* <RTS> identifies the Ada runtime library to be used, (either a GNAT Pro
  provided runtime, identified by its name, e.g. embedded-stm32f4, or the path
  to a custom runtime project);
* <TRACEi> is the name of an output file containing an execution trace;
* <ROOT_PROJECT> is the path to the GPR project file defining the main units for
  the application testing;
* <UNITS> is a specification of the units for which coverage is to be
  assessed (so called *Units Of Interest*), with GPR project file facilities,
  as documented in the tool User's Guide and summarized in the |tor_doc|
  document;

* <traces.list> is a text file containing the list of execution traces to
  operate on.

IO redirections, sometimes necessary for proper operation of the programs
within the execution environment, are allowed.
