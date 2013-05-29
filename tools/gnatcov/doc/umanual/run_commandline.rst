.. index::
   single: gnatcov run command line

.. _gnatcov_run-commandline:

**********************
|gcvrun| command line
**********************

|gcvrun| offers a unified interface to launch programs for a specific target
machine and produce execution traces. The general interface synopsis is
available from ``gnatcov`` ``--help``, as follows::

   run [OPTIONS] [EXE] [-eargs EARGS...]

:option:`EXE`:
  The executable program to be emulated. This provided name is stored in
  the output trace header, where it is retrieved later by |gcvcov| for
  analysis purposes. Relative paths will be considered relative to the
  location where |gcvcov| is launched. When :ref:`eargs <eargs>` are passed,
  the executable may be provided there instead.

.. _eargs:
:option:`-eargs` :
  Pass what follows to the low-level machine simulator that eventually
  executes the program. If the executable program to run is not provided
  elsewhere on the command line, the first earg is used for this purpose.

Then the available :option:`[OPTIONS]` are:

:option:`--kernel` :
  Tell the underlying emulator that the executable program actually
  is a module to be loaded on top of the provided kernel binary. This is
  typically for VxWorks kinds of targets, where the kernel is a tailored
  version built to include GNATemulator support.

:option:`-t`, :option:`--target` :
  The target architecture/board/abi that your program was built for. This
  typically corresponds to the target prefix of your compilation toolchain,
  for example ``powerpc-elf`` or ``leon-elf``. By default, |gcv| assumes
  this is the same as its host environment. Further explanation is provided
  below, after the options' descriptions.

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

:option:`--level` :
  Convey the most precise kind of analysis that is intended from the produced
  traces later on. This defaults to `stmt+decision` and is best combined with
  :option:`-P` or :option:`--scos` for efficiency when set to `stmt+mcdc`. See
  the :ref:`trace-control` section of this chapter for additional details.

When |gem| is available on your PATH as `<target>-gnatemu` (`<target>` is the
value provided as the :option:`--target` argument), |gcp| uses it to
run your program. |gem| acts as a wrapper around the real machine emulator in
this case, taking care of low-level interfacing details. Otherwise, |gcp|
resorts directly to the low level emulator statically configured for your
:option:`--target` argument (when the tool was built), if any, and if it is
available on your PATH.

The :option:`-eargs` command line options that |gcvrun| receives are passed
straight to the low-level emulation engine in both cases.  They are not
interpreted by |gem| when it is used. In native configurations, without an
intermediate emulation engine, the eargs are passed as command line arguments
to the executable program.

Here are a few examples of valid command lines. The simplest possible first::

  gnatcov run --target=powerpc-elf myprog
  # Run "myprog" using powerpc-elf-gnatemu as the execution environment.
  # Produce myprog.trace in the current directory.

  gnatcov run --target=powerpc-elf myprog -o myrun.trace
  # Likewise, producing myrun.trace instead, still in the current directory

  gnatcov run --target=powerpc-elf myprog -o myrun.trace -eargs --version
  # Likewise, also requesting version output from the low level execution
  # engine, *not* from gnatemulator if it happens to be involved.

  gnatcov run --target=powerpc-elf -o myrun.trace -eargs myprog --version
  # Likewise, providing the executable program to run as the first earg

  gnatcov run --target=powerpc-elf myprog -T "trace for doc example"
  # Providing a trace tag, that can de retrieved with trace dump facilities
  # and which is displayed in some output reports.

  gnatcov run -eargs myprog arg1 arg2
  # Where supported, run "myprog" in the native environment through an
  # instrumentation layer to produce the execution trace. Pass arg1 and arg2
  # as command line arguments to "myprog".

When MCDC analysis is intended, as indicated with :option:`--level=stmt+mcdc`,
a few options are available to designate the source units of interest,
allowing optimal trace generation for more efficient processing:

:option:`-P` :
   Use indicated project file as the root project for operations that need
   locating information about units to analyze. Default options are taken from
   this project, and all the projects listed in :option:`--projects` switches
   must be imported by the root project.
 
:option:`--projects`, |rarg| :
   Within the dependency closure of the root project designated by :option:`-P`,
   designate projects on which to focus in particular.

:option:`--recursive` : 
   When using :option:`-P` and :option:`--projects` to control operations,
   consider the dependency closure of all the designated projects.

   See the :ref:`using-gpr` section for extra details and use examples of
   :option:`--P`, :option:`--projects` and :option:`--recursive`.

:option:`--units`, |rarg| :
   When using project files, override the list of units of interest for
   source coverage.

:option:`--subdirs` :
   When using project files, look for :term:`Library Information files` in the
   indicated subdirectory of each project's object directory.

:option:`--scos`, |rarg| :
   For source coverage analysis specifically, provide the set of Library
   Information files from which SCOs should be loaded. This low-level switch
   effectively overrides the selection of units of interest for source
   coverage, in particular bypassing project-based unit selection based on
   switches :option:`-P` and :option:`--units`.

See :ref:`trace-control` for more details on the influence of these
options, and :ref:`sunits` for extra information and examples describing
their use.

