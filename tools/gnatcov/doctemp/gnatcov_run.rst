**********************
Instrumented Execution
**********************

@pindex gnatcov run

|gcvrun| command line
======================

|gcvrun| offers a unified interface to launch programs for a specific
target machine and produce execution traces.

The general interface synopsis is available from *gnatcov --help*,
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
  

*-v* requests verbose output, in particular of the commands to
run the program within the underlying instrumented environment.

The `FILE` argument is the executable program file name.
This name is stored as-provided in the output trace header, where it is
retrieved later by |gcvcov| for analysis purposes.

By default, |gcvrun| writes the execution trace in the current directory,
in a file named like the executable input with a `.trace` suffix.
For example |gcvrun| `/path/to/myexecfile` produces a
`myexecfile.trace` file in the current directory.
*--output* allows the selection of an alternate output file name.

@pindex `Trace tags`
The *--tag* option expects a string argument and stores it
verbatim as a trace tag attribute in the output trace header.
The tag so associated with a trace can be retrieved from trace dumps
and is output as part of some analysis reports.
It is useful as a flexible trace identification facility, structured as
users see fit for custom trace management purposes.

*--level* conveys the kind of analysis that is intended from
the produced traces later on.
*--scos*, possibly repeated and accepting @ arguments, conveys
the set of Source Coverage Obligations (SCOs) intended to be assessed
from the produced traces.
These two options are only needed for MCDC analysis purposes.
See the Trace control options section below for more details.

Finally, the *-eargs* options passes what follows it to the
low-level machine simulator that eventually executes the program.

Emulator selection and control
==============================

When |gem| is available on your PATH (as `<target>-gnatemu`),
|gcp| uses it to run your program.
|gem| acts as a wrapper around the real machine emulator in this
case, taking care of low-level interfacing details that are irrelevant
to users.

Nevertheless, |gcp| also knows about a few low-level emulators
itself and will resort to one when |gem| happens to be unavailable.

The *-eargs* command line options that |gcvrun| receives are
passed straight to the low-level emulation engine in both cases.
They are not interpreted by |gem| when it is used, just passed by it
to the underlying machine simulator.

Trace control options for MCDC
==============================

MCDC analysis using execution traces requires specific care to make
sure that assessments are both accurate and performant.
With |gcp|, this is achieved by the combination of two options passed
to |gcvrun|:

* 
  `--level=stmt+mcdc` to activate the collection of object branch
  histories,

* 
  `--scos=@<ALIs>` to convey the set of SCOs that will be subject
  to MCDC analysis, allowing to focus the branch history collections
  on the critical branches only.
  History is known to only be required for a particular subset of conditional
  branches involved in decisions 

Traces with history may be used to assess other criteria than MCDC.
The opposite is not true: MCDC assessment is only accurate when branch
history was turned on for decisions that need it.

In absence of `--scos`, history is activated for all the object
conditional branch instructions, resulting in larger traces increased
processing time.

@page

