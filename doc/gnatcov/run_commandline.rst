.. index::
   single: gnatcov run command line

.. _gnatcov_run-commandline:

**********************
|gcvrun| command line
**********************

|gcvrun| offers a unified interface to launch programs for a specific target
machine and produce execution traces. The general interface synopsis is
available from ``gnatcov`` ``--help``, as follows::

   gnatcov run [OPTIONS] [EXE] [-eargs [EXE] EARGS...]

.. _exe:

:cmd-option:`EXE` :
  Name of the program file to execute. The provided path is stored in the
  output trace header that |gcvcov| fetches for analysis purposes later on.
  Relative paths there will be considered relative to the location where
  |gcvcov| is launched. If the program name is not provided on the base
  command line (before eargs), the first earg or a project file attribute can
  be used for this purpose instead. See the :ref:`exe-and-args` section of
  this chapter for more details on this process.

.. _eargs:

:cmd-option:`-eargs` :
  For cross configurations (with :cmd-option:`--target`), pass the items not
  used as the program name to the machine simulator that eventually executes
  the program. For native configurations, pass the items not used as the
  program name as command line arguments to the executed program.

Then the available :cmd-option:`[OPTIONS]` are:

:cmd-option:`--level` :
  Convey the most precise kind of analysis that is intended from the produced
  traces later on. This defaults to ``stmt+decision`` and must be set to
  ``stmt+mcdc`` if you intend to perform MCDC analysis. In this case, it is
  highly recommended, for efficiency purposes, to also state the units on which
  you will perform such analysis with :cmd-option:`-P` or :cmd-option:`--scos`
  options, as described in :ref:`sunits`.

:cmd-option:`-t`, :cmd-option:`--target` :
   .. include:: target_switch_common_text.rst

   This is used to control the underlying execution engine used to run
   the program, assumed to be the host environment by default. On the
   command line, a possible ``,<board-name>`` extension is allowed as
   well. See the :ref:`execution-control` section of this chapter for
   additional details.


:cmd-option:`--kernel` :
  Tell the underlying emulator that the executable program actually
  is a module to be loaded on top of the provided kernel binary. This is
  typically for VxWorks kinds of targets, where the kernel is a tailored
  version built to include GNATemulator support.

:cmd-option:`-v`, :cmd-option:`--verbose` :
  Request verbose output. In particular, this displays the commands launched
  to run the program within the underlying instrumented environment.

:cmd-option:`-o`, :cmd-option:`--output` :
  Request the selection of an alternate output file name for the execution
  trace. Otherwise, |gcvrun| writes the execution trace in the current
  directory, in a file named like the executable input with a ``.trace``
  suffix.  For example |gcvrun| `/path/to/myexecfile` produces a
  `myexecfile.trace` file in the current directory by default.

:cmd-option:`-T`, `--tag` :
  Store the provided string argument verbatim as a trace tag attribute in the
  output trace header.  The tag so associated with a trace can be retrieved
  from trace dumps and is output as part of some analysis reports.

:cmd-option:`-P` :
   Designate a root project file for various possible purposes.  This can first
   be used together with :cmd-option:`--projects` as an alternative to
   :cmd-option:`--scos` to collect the set of units of interest for later
   coverage assessments if mcdc computation is needed. See the
   :ref:`mcdc-examples` and :ref:`sunits` sections of this manual for more
   details on this. Another possible use is the specification in the root
   project file of attributes controlling the execution, such as the coverage
   level intended to be assessed later on or the name of the main subprogram
   unit, from which the name of the executable to run can be inferred. In such
   cases, project attributes provide default values, ignored when the
   corresponding item is specified on the command line.
