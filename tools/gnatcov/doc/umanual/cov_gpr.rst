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

1. Specify default switches for the various |gcv| commands,

2. Select units of interest and retrieve Source Coverage Obligations
   for source coverage analysis,

3. Retrieve exemption regions for source and object coverage analysis,

4. Specify the target architecture for which the analyzed program was built.
  
A common set of rules apply in all cases:

* A single root project file is specified on the command line using
  :option:`-P`,

* :option:`--projects` options might be added to designate specific projects
  to operate on within the root dependency closure. As soon as one such option
  is used, the root project itself needs to be listed explicitely as well to
  be considered.

* With :option:`--recursive` anywhere in addition, the set of projects to be
  processed includes the transitive closure of all the projects designated by
  :option:`-P` and :option:`--projects` if any.

If only the root project file is specified using :option:`-P` and if this
project has an ``Origin_Project`` attribute, the project that is actually
considered for coverage assessment is the one designated by this attribute.
``Origin_Project`` is ignored in all other cases.

A ``Coverage`` package is allowed in every project file to provide attributes
guiding the analysis in different possible ways. Two families of attributes
are available today:

* A ``Switches`` family allowing the specification of options to apply for the
  various |gcv| commands involved in an execution/analysis sequence. This is
  the core facility regarding point 1 above, covered in the :ref:`switches_attr`
  section below.

* A ``Units`` family, allowing fine grain selection of source units to
  consider within a project tree, an additional item to help fulfill point 2
  above.

The project selection facilities are illustrated together with the fine grain
unit selection devices in the :ref:`sunits` section, focused on source coverage
perspectives.

.. _switches_attr:

Specifying command Switches from project files
==============================================

``Switches`` attributes in the root project file are treated as lists of
command line switches for |gcv| commands. Each attribute specification
requires an index indicating what |gcv| operation the switches apply to.  Each
attribute value is expected to be a list of options for this operation.  Here
is a first basic example::

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
list cumulates before ``--annotate`` for |gcvcov|. Similarily, switches from
the project file are always processed as if appearing before the others on the
command line.

For switches like :option:`--level` that don't accumulate to produce sets, the
last occurrence on the command line prevails. The project file values act as
defaults that can be overriden by an explicit value on the command line,
wherever they are placed (before or after :option:`-P`).

For switches such as :option:`--units` which have cumulative effect, later
occurrences on the command line add up with, rather than replace, those
specified in the project file.

.. _target_attr:

Specifying the Target from project files
========================================

Similarly to other tools, |gcv| uses any existing ``Target`` attribute in the
root project file in order to detect what target architecture to consider. This
can be done instead of providing the :option:`--target` option both for correct
processing of project files and to run the appropriate execution environment in
|gcvrun|.  Here is a simple example::

    project My_Program is
       for Languages use ("Ada");
       for Main use ("my_program.adb");

       for Target use "powerpc-elf";

       package Compiler is
          for Default_Switches ("Ada") use
            ("-g", "-fdump-scos", "-fpreserve-control-flow");
       end Compiler;
    end My_Program;

When the root project provides a ``Target`` attribute and |gcv| is passed a
:option:`--target` option on the command line, the option takes precedence over
the attribute.
