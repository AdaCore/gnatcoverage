.. _using-gpr:

*******************
Using project files
*******************

General considerations
======================

GNAT project files are a useful device to describe the structure of larger
applications, for the benefit of the tools intervening at various stages of
development, from the IDE to build and analysis tools.

|gcp| takes full advantage of GNAT projects for various aspects of the
coverage analysis activity, in particular:

#. Select units of interest and retrieve Source Coverage Obligations
   for source coverage analysis,

#. Retrieve exemption regions for source and object coverage analysis,

#. Specify default switches for the various |gcv| commands,

#. Specify the target architecture and runtime profile for which the analyzed
   program was built.

The project selection and units-of-interest designation facilities are
described in the :ref:`sunits` chapter of this manual, with a focus on source
coverage perspectives. In the root project, a ``Coverage`` package may be used
to specify the arguments for |gcv| commands and the target/runtime profile may
be set using toplevel attributes.

Please note that **non-library aggregate project files are currently not
supported** and will be rejected by |gcv|. The aggregated projects will have
to be processed individually and coverage reports consolidated
:ref:`through checkpoints<checkpoints>`, when this makes sense.

.. _switches_attr:

Specifying |gcv| command Switches
=================================

``Switches`` attributes in ``Coverage`` package of the root project file are
treated as lists of command line switches for |gcv| commands. Each attribute
specification requires an index indicating what |gcv| operation the switches
apply to.  Each attribute value is expected to be a list of options for this
operation.  Here is a first basic example::

    package Coverage is
       level := "--level=stmt"; -- to be reused in different contexts

       for Switches ("run") use (level);
       -- This will apply to "gnatcov run"

       for Switches ("coverage") use (level, "--annotate=report");
       -- This will apply to "gnatcov coverage"
    end Coverage;

For switches applicable to all the commands you are planning to use, the
special ``"*"`` index is available to denote `any` command. If you are going
to use only ``run`` and ``coverage``, for instance, the example above might be
re-written as::

    package Coverage is
       for Switches ("*") use ("--level=stmt");
       for Switches ("coverage") use ("--annotate=report");
    end Coverage;

The ``*`` arguments are always inserted first with respect to the final
command line interpretation. In the example above, ``--level`` from the ``*``
list cumulates before ``--annotate`` for |gcvcov|. Similarly, switches from
the project file are always processed as if appearing before the others on the
command line.

For switches like :cmd-option:`--level` that don't accumulate to produce sets,
the last occurrence on the command line prevails. The project file values act
as defaults that can be overriden by an explicit value on the command line,
wherever they are placed (before or after :cmd-option:`-P`).

For switches such as :cmd-option:`--units` which have cumulative effect, later
occurrences on the command line add up with, rather than replace, those
specified in the project file.

.. _target_attr:

Specifying the Target and language Runtime
==========================================

Similarly to other tools, |gcv| uses any existing ``Target`` or ``Runtime``
attribute in the root project file to detect what target architecture and
associated language runtime profile to consider. This can be done instead of
providing the :cmd-option:`--target` and :cmd-option:`--RTS` options both for
correct processing of project files and to run the appropriate execution
environment in |gcvrun|.  Here is a simple example::

    project My_Program is
       for Languages use ("Ada");
       for Main use ("my_program.adb");

       for Target use "powerpc-elf";
       for Runtime use "zfp-mpc8641";
    end My_Program;

When a :cmd-option:`--target` or a :cmd-option:`--RTS` option is provided on
the command line, the option takes precedence over the corresponding attribute.
