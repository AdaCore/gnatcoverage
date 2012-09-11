.. index::
   single: gnatcov run command line

.. _gnatcov_run-commandline:

**********************
|gcvrun| command line
**********************

|gcvrun| offers a unified interface to launch programs for a specific target
machine and produce execution traces. The general interface synopsis is
available from ``gnatcov`` ``--help``, as follows::

   run [OPTIONS] FILE [-eargs EARGS...]
     Options are:
     -t TARGET  --target=TARGET   Set the target
       targets: powerpc-elf leon-elf leon3-elf powerpc-wrs-vxworks
     -v --verbose                 Be verbose
     -T TAG  --tag=TAG            Put TAG in tracefile
     -o FILE  --output=FILE       Write traces to FILE
     --level=CRIT                 Assume CRIT as the strictest future
                                  analysis criterion
     --scos=FILE                  Add FILE to the set of SCOS
     -eargs EARGS                 Pass EARGS to the low-level emulator
     --kernel=FILE                Specify which kernel to use
  
:option:`FILE` |marg| :
  The executable program to be emulated. This provided name is stored in
  the output trace header, where it is retrieved later by |gcvcov| for
  analysis purposes. Relative paths will be considered relative to the
  location where |gcvcov| is launched.

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
  Convey the kind of analysis that is intended from the produced traces later
  on. This is actually only useful for MCDC analysis, combined with
  :option:`--scos`.  See the :ref:`trace-control` section of this chapter for
  additional details.

:option:`-P`:
   Use indicated project file as the root project. Default options are taken
   from this project. All projects listed in --projects switches must be
   imported by the root project.
 
:option:`--projects`, |rarg|:
   When using project files, consider units of interest from the given
   projects.

:option:`--recursive`: 
   When using project files to identify units of interest for source coverage,
   also consider imported projects.

   See section :ref:`sunits` for extra details and use examples.

:option:`--units`, |rarg|:
   When using project files, override the list of units of interest for
   source coverage.

:option:`--subdirs`:
   When using project files, look for Library Information files in the
   indicated subdirectory of each project's object directory.

:option:`--scos`, |rarg|:
   For source coverage analysis specifically, provide the set of Library
   Information files from which SCOs should be loaded. This low-level switch
   effectively overrides the selection of units of interest for source
   coverage, in particular bypassing project-based unit selection based on
   switches :option:`-P` and :option:`--units`.

See section :ref:`sunits` for extra details and use examples about the
various switches used to specify units of interest for source coverage.
Note that as :option:`--level`, in the case of |gcvrun| these options are
useful only in the case of MCDC analysis.

:option:`-eargs` :
  Pass what follows to the low-level machine simulator that eventually
  executes the program.

:option:`--kernel` :
  Tell the underlying emulator that the executable program actually
  is a module to be loaded on top of the provided kernel binary. This is
  typically for VxWorks kinds of targets, where the kernel is a tailored
  version built to include GNATemulator support.

When |gem| is available on your PATH as `<target>-gnatemu` (`<target>` is the
value provided as the :option:`--target` argument), |gcp| uses it to
run your program. |gem| acts as a wrapper around the real machine emulator in
this case, taking care of low-level interfacing details. Otherwise, |gcp|
resorts directly to the low level emulator statically configured for your
:option:`--target` argument (when the tool was built), if any, and if it is
available on your PATH.

The :option:`-eargs` command line options that |gcvrun| receives are
passed straight to the low-level emulation engine in both cases.
They are not interpreted by |gem| when it is used.

Here are a few examples of valid command lines. The simplest possible first::

  gnatcov run myprog
  # determine the target architecture, then run myprog in the corresponding
  # instrumented environment. Produce myprog.trace in the current directory.

  gnatcov run myprog -o myrun.trace
  # Likewise, producing myrun.trace instead, still in the current directory

  gnatcov run myprog -o myrun.trace -eargs -v
  # Likewise, also requesting verbose output from the low level execution
  # engine, *not* from gnatemulator if it happens to be involved.

  gnatcov run myprog -T "trace for documentation example"
  # Providing a trace tag, that can de retrieved with trace dump facilities
  # and which is displayed in some output reports.
