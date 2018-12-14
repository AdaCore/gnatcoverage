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

.. _exe:

:option:`EXE` :
  Name of the program file to execute. The provided path is stored in the
  output trace header that |gcvcov| fetches for analysis purposes later on.
  Relative paths there will be considered relative to the location where
  |gcvcov| is launched. If the program name is not provided on the base
  command line (before eargs), the first earg or a project file attribute can
  be used for this purpose instead. See the :ref:`exe-and-args` section of
  this chapter for more details on this process.

.. _eargs:

:option:`-eargs` :
  For cross configurations (with :option:`--target`), pass the items not used
  as the program name to the machine simulator that eventually executes the
  program. For native configurations, pass the items not used as the program
  name as command line arguments to the executed program.

Then the available :option:`[OPTIONS]` are:

:option:`--level` :
  Convey the most precise kind of analysis that is intended from the produced
  traces later on. This defaults to `stmt+decision` and is best combined with
  :option:`-P` or :option:`--scos` for efficiency when set to `stmt+mcdc`.

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

:option:`-P` :
   Designate a root project file for various possible purposes.  This can
   first be used together with :option:`--projects` as an alternative to
   :option:`--scos` to collect the set of units of interest for later coverage
   assessments if mcdc computation is needed. See the :ref:`trace-control` and
   :ref:`sunits` sections of this manual for more details on this. Another
   possible use is the specification in the root project file of attributes
   controlling the execution, such as the coverage level intended to be
   assessed later on or the name of the main subprogram unit, from which the
   name of the executable to run can be inferred. In such cases, project
   attributes provide default values, ignored when the corresponding item is
   specified on the command line.
