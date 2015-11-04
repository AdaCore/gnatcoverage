.. index::
   single: gnatcov run command line

.. _gnatcov_run-commandline:

**********************
|gcvrun| command line
**********************

|gcvrun| offers a unified interface to launch programs for a specific target
machine and produce execution traces. The general interface synopsis is
available from ``gnatcov`` ``--help``, as follows::

   run [OPTIONS] [EXE] [-eargs [EXE] EARGS...]

:option:`EXE` :
  The executable program to be emulated. The provided path is stored in the
  output trace header that |gcvcov| fetches for analysis purposes later on.
  Relative paths there will be considered relative to the location where
  |gcvcov| is launched. When :ref:`eargs <eargs>` are passed, the executable
  name may be provided there instead.

.. _eargs:

:option:`-eargs` :
  For cross configurations (with :option:`--target`), pass what follows to the
  machine simulator that eventually executes the program. For native
  configurations, pass what follows as command line arguments to the executed
  program. If the executable program to run is not provided elsewhere on the
  command line, the first earg is used for this purpose.

Then the available :option:`[OPTIONS]` are:

:option:`--level` :
  Convey the most precise kind of analysis that is intended from the produced
  traces later on. This defaults to `stmt+decision` and is best combined with
  :option:`-P` or :option:`--scos` for efficiency when set to `stmt+mcdc`. See
  the :ref:`trace-control` section of this chapter for additional details.

:option:`-t`, :option:`--target` :
  State the target architecture/board/abi for which the analyzed program was
  built.  This corresponds to the target prefix of your compilation toolchain,
  for example ``powerpc-elf`` or ``leon-elf``, and can also be specified as a
  ``Target`` attribute within the project file designated by -P, if any. This
  is used to control the underlying execution engine used to run the program,
  assumed to be the host environment by default. On the command line, a
  possible ``,<board-name>`` extension is allowed as well. See the
  :ref:`execution-control` section of this chapter for additional details.

:option:`--kernel` :
  Tell the underlying emulator that the executable program actually
  is a module to be loaded on top of the provided kernel binary. This is
  typically for VxWorks kinds of targets, where the kernel is a tailored
  version built to include GNATemulator support.

:option:`-v`, :option:`--verbose` :
  Request verbose output. In particular, this displays the commands launched
  to run the program within the underlying instrumented environment.

:option:`-o`, :option:`--output` :
  Request the selection of an alternate output file name for the execution
  trace. Otherwise, |gcvrun| writes the execution trace in the current
  directory, in a file named like the executable input with a ``.trace``
  suffix.  For example |gcvrun| `/path/to/myexecfile` produces a
  `myexecfile.trace` file in the current directory by default.

:option:`-T`, `--tag` :
  Store the provided string argument verbatim as a trace tag attribute in the
  output trace header.  The tag so associated with a trace can be retrieved
  from trace dumps and is output as part of some analysis reports.

