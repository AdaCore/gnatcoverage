.. _execution-control:

*********************
Execution environment
*********************

For cross configurations, the :option:`--target` command line option allows
specifying the target environment for which the program was built and for
which |gcvrun| should pick a suitable execution environment. The option states
a base target name possibly followed by a board specialization after a
separating '``,``' (:option:`--target=powperpc-eabispe,mpc5566` for
example). When a simple target name alone is sufficient, it is possible to let
|gcv| detect it from the ``Target`` attribute from the project file (see
:ref:`target_attr` section). Then:

* When |gem| for the base target is available on your PATH, as
  `<base-target>-gnatemu`, |gcv| uses this to run the program. |gem| acts as a
  wrapper around the real machine emulator in this case, taking care of
  low-level interfacing details. If an optional board extension is provided in
  the :option:`--target` argument, the specified board name is passed as an
  extra :option:`--board=<board-name>` command line option to |gem|.

* Otherwise, |gcv| resorts to a builtin low level emulator statically
  configured for the base target. An ``unsupported target`` error is issued
  and |gcv| exits if no such configuration is found to match.

.. _exe-and-args:

***************************************
Program selection and Arguments passing
***************************************

To facilitate the coverage analysis integration within different kinds of
environments, |gcvrun| supports several ways of selecting the program to
execute:

* If an :ref:`[EXE] <exe>` argument is provided on the base command line, before
  :option:`eargs`, use it as the candidate program name to execute. Otherwise:

* If :ref:`eargs <eargs>` are provided and the first item designates an
  executable file, use this as the program to execute. Otherwise:

* If a root project file is designated with :option:`-P`, and this project
  features a single Main attribute, use the corresponding executable name as
  the program to execute.

The :option:`eargs` switches not used as the program name are then used
differently in cross or native configurations, as differentiated by the
use of a :option:`--target` option.

In cross configurations, :option:`eargs` switches are passed straight to the
low-level emulation engine.  They are not interpreted by |gem| when it is
used. Passing arguments to |gem| via |gcvrun| can be achieved by way of a GNAT
project file, with a Switches attribute in the Emulator package. For example::

  -- covemu.gpr
  project Covemu is
    package Emulator is
      for Switches use ("--gnatbus=bridge.bar.com:1800");
    end Emulator;
  end Covemu;

Then::

  gnatcov run --target=<target> -Pcovemu.gpr

In native configurations, the program executes in the host environment and the
:option:`-eargs` switches are passed as command line arguments to the
executable program.

This way users can just prefix a regular host command line by "gnatcov run
... -eargs" to produce execution traces, as illustrated by the native case in
the set of valid |gcv| command line examples below::

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

  gnatcov run --target=powerpc-elf -o myrun.trace -Pmyprj.gpr -eargs --version
  # Likewise, inferring the executable program name from the Main attribute
  # in myprj.gpr.

  gnatcov run --target=powerpc-elf myprog -T "trace for doc example"
  # Providing a trace tag, that can de retrieved with trace dump facilities
  # and which is displayed in some output reports.

  gnatcov run --target=powerpc-eabispe myprog
  # Run "myprog" using powerpc-eabispe-gnatemu as the execution environment.
  # Produce myprog.trace in the current directory.

  gnatcov run --target=powerpc-eabispe,mpc5566 myprog
  # Likewise, instructing gnatemu to select the "mpc5566" board emulation.

  gnatcov run -eargs myprog arg1 arg2
  # Where supported, run "myprog" in the native environment through an
  # instrumentation layer to produce the execution trace. Pass arg1 and arg2
  # as command line arguments to "myprog".
