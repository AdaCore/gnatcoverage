.. index::
   single: gnatcov convert command line

.. _gnatcov_convert-commandline:

**********************
|gcvcnv| command line
**********************

|gcvcnv| converts trace data from foreign sources to the format used by
|gcvcov|.  The general interface synopsis is available from ``gnatcov``
``--help``::

   gnatcov convert --trace-source=SOURCE_ID --exec=EXECUTABLE
     --input=INPUT_TRACE --hw-trigger-traces=START_ID,START_ADDR,STOP_ID
     [OPTIONS]

Details about the available options follow:

:option:`--trace-source` :
  An identifier of the source of the input trace data is provided using
  this option.  The input source effects which other options are required.
  Currently, the only value for this option is ``iSystem-5634``, which is
  the identification for Nexus data output by the MPC5634M SOC, captured
  and written by an iSystem probe and software. For the ``iSystem-5634``
  source, the ``--hw-trigger-traces`` option is required.

:option:`--exec` :
  This option provides the filename of a file containing the
  target executable from which the trace data was generated, and upon
  which the coverage analysis is done.

:option:`--input` :
  This option provides the name of the file containing the input trace
  data to be converted.

:option:`--output` :
  The name of the file to write the output trace into can be provided by
  by this option. If ``--output`` is not used, the output filename is derived
  from the name of the executable, by taking the basename of the executable
  file and appending ``.trace``. 

:option:`--hw-trigger-traces` :
  For Nexus modules that support the Class 4 HW Watchpoint Trigger feature,
  the generation of program trace data can be started and stopped
  in response to watchpoint events. When this mechanism is used, in order
  to interpret the Nexus traces, information about how the triggers were
  set during the run of the executable needs to be provided. For the ``iSystem-5634``
  trace-source, |gcvcnv| allows the use of the Instruction Address
  Compare registers for the ``start/stop`` watchpoints. ``START_ID``
  is one of ``IAC1``, ``IAC2``, ``IAC3``, or ``IAC4``, and ``START_ADDR``
  is the address set in the IAC identified by ``START_ID``. The address
  can be specified numerically using either the usual C syntax (0xHHHH),
  Ada syntax (16#HHHH_HHHH#), or by providing an ELF symbol table symbol
  name (e.g. ``main``). ``STOP_ID`` can identify another of the IACs, or be
  ``0``, to indicate that a stop trigger was not set up.

:option:`--level` :
  This is used in an identical manner as for |gcvrun|.

:option:`--scos` :
  This is used in an identical manner as for |gcvrun|.
