**********************
Instrumented Execution
**********************

.. index:
   :single: gnatcov run

|gcvrun| offers a unified interface to launch programs for a specific
target machine and produce execution traces.

.. _gnatcov_run-commandline:

|gcvrun| command line
=====================

The general interface synopsis is available from ``gnatcov`` ``--help``,
as follows:

::

   run [OPTIONS] FILE [-eargs EARGS...]
     Options are:
     -t TARGET  --target=TARGET   Set the target
       targets: powerpc-elf leon-elf i386-pok i386-linux prepare
     -v --verbose                 Be verbose
     -T TAG  --tag=TAG            Put TAG in tracefile
     -o FILE  --output=FILE       Write traces to FILE
     --level=CRIT                 Assume CRIT as the strictest future
                                  analysis criterion
     --scos=FILE                  Add FILE to the set of SCOS
     -eargs EARGS                 Pass EARGS to the low-level emulator
  

:option:`-t`, :option:`--target` : The target architecture/board/abi that your
program was built for. This typically corresponds to the target prefix of your
compilation toolchain, for example ``powerpc-elf`` or ``leon-elf``. |gcv|
knows how to infer this from the executable header most of the time.

:option:`-v`, :option:`--verbose` : Request verbose output. In particular,
this displays the commands launched to run the program within the underlying
instrumented environment.

:option:`-o`, :option:`--output` : Request the selection of an alternate
output file name for the execution trace. Otherwise, |gcvrun| writes the
execution trace in the current directory, in a file named like the executable
input with a ``.trace`` suffix.  For example |gcvrun| `/path/to/myexecfile`
produces a `myexecfile.trace` file in the current directory by default.

:option:`-T`, `--tag` : Store the provided string argument verbatim as a trace
tag attribute in the output trace header.  The tag so associated with a trace
can be retrieved from trace dumps and is output as part of some analysis
reports.  It is useful as a flexible trace identification facility, structured
as users see fit for custom trace management purposes.

:option:`--level` : Convey the kind of analysis that is intended from the
produced traces later on. This is actually only useful for MCDC analysis,
combined with :option:`--scos`.  See the :ref:`trace-control` section of
this chapter for additional details.

:option:`--scos`, |rarg| : Convey the set of :term:`Source Coverage Obligations`
(SCOs) intended to be assessed from the produced traces. As :option:`--level`,
this option is only useful for MCDC analysis, as described in greater details
in the :ref:`trace-control` section later in this chapter.

:option:`-eargs` : Pass what follows to the low-level machine simulator
that eventually executes the program.

:option:`FILE` *(mandatory)* : The executable program to be emulated. This
name is stored as-provided in the output trace header, where it is retrieved
later by |gcvcov| for analysis purposes. Relative paths will be considered
relative to the location where |gcvcov| is launched, not relative to where the
trace file is located.

Emulator control
================

When |gem| is available on your PATH as `<target>-gnatemu`, |gcp| uses it to
run your program. |gem| acts as a wrapper around the real machine emulator in
this case, taking care of low-level interfacing details that are irrelevant to
users.

Otherwise, |gcp| resorts directly to the low level emulator configured for
your :option:`--target` argument, if any and if it is available on your PATH.

The :option:`-eargs` command line options that |gcvrun| receives are
passed straight to the low-level emulation engine in both cases.
They are not interpreted by |gem| when it is used.

.. _trace-control:

Trace control for MCDC
=======================

MCDC analysis using execution traces requires specific care to make
sure that assessments are both accurate and efficient.
With |gcp|, this is achieved by the combination of two options passed
to |gcvrun|:

* :option:`--level=stmt+mcdc` to activate the collection of object branch
  histories, chronological record of the directions taken at conditional
  branch points in the machine code,

* :option:`--scos=@<list-file>` to convey the set of SCOs that will be subject
  to MCDC analysis, asking |gcv| to focus the branch history collections
  on the critical branches only.

MCDC assessment is only accurate when branch history was turned on for
decisions that require it, which |gcv| knows to determine from SCOs.  With
:option:`--level=stmt+mcdc` and in absence of :option:`--scos`, history is
activated for all the object conditional branch instructions, resulting in
larger traces and increased processing time compared to what is strictly
needed. Providing SCOs instructs |gcv| to restrict history collections to
branches that need it, allowing optimized operation downstream.  Care must be
taken in this case not to query MCDC analysis on SCOs that were not included
in the set provided to |gcvrun|.

Statement or decision coverage assessments, conversely, can be performed with
any kind of trace, so traces with history aimed at MCDC may be used for those
other criteria as well.
