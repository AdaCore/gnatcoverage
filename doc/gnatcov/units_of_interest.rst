.. _sunits:

Specifying *Units Of Interest*
==============================

This chapter describes the means available to convey the set of units on which
coverage should be assessed, which we commonly refer to as the :dfn:`units of
interest`, and which are relevant to |gcvins|, |gcvrun| and |gcvcov|.

There are two main families of such means: users would either provide the set
of files which hold the coverage obligations for the units of interest, or
rely on project files facilities to designate the set of units. At analysis
time, the actual list of unit names for which a report or checkpoint is
produced as well as the list of individually ignored source files for each unit
can be displayed with the :cmd-option:`--dump-units-to` option of the
|gcvcov| command.

.. _passing_scos:

Providing coverage obligation files (:cmd-option:`--scos|--sid`)
----------------------------------------------------------------

With the :cmd-option:`--scos` or :cmd-option:`--sid` command line arguments,
users convey the set of units of interest by directly providing the set of
files which contain the coverage obligations for those units.

The :cmd-option:`--scos` switch is for binary trace based analysis and the
files to provide are the *Library Information* files produced by the compiler
(``.ali`` files for Ada, ``.gli`` files for C). The :cmd-option:`--sid` switch
is for source trace based analysis and the files to provide are the ``.sid``
*Source Instrumentation Data* files produced by ``gnatcov instrument``. In all
cases, the files are located at the same place as where the object file for a
unit is produced.

The following paragraphs provide details and examples on the use of
:cmd-option:`--scos` with ``.gli`` or ``.ali`` files. The same principles apply
to :cmd-option:`--sid` with ``.sid`` files.

Each occurrence of :cmd-option:`--scos` on the command line expects a single
argument which specifies a set of units of interest. Multiple occurrences are
allowed and the sets accumulate. The argument might be either the name of a
single Library Information file for a unit, or a :term:`@listfile arguments
<@listfile argument>` expected to contain a list of such file names.

For example, focusing on Ada units ``u1``, ``u2`` and ``u3`` can be achieved
with either ``--scos=u1.ali --scos=u2.ali --scos=u3.ali``, with ``--scos=u3.ali
--scos=@lst12`` where ``lst12`` is a text file containing the first two ALI
file names, or with other combinations alike.

The GNAT toolchain provides a useful device for list computations: the
:cmd-option:`-A` command line argument to :command:`gnatbind` which produces a
list of all the ``.ali`` files involved in an executable construction.  By
default, the list goes to standard output. It may be directed to a file on
request with :cmd-option:`-A=<list-filename>`, and users may of course filter
this list as they see fit depending on their analysis purposes.

Below is an example sequence of commands to illustrate, using the standard Unix
``grep`` tool to filter out test harness units, assuming a basic naming
convention::

    # Build executable and produce the corresponding list of ALI files. Pass
    # -A to gnatbind through gprbuild -bargs then filter out the test units:
    gprbuild -p --target=powerpc-elf --RTS=zfp-prep -Pmy.gpr
     test_divmod0.adb -fdump-scos -g -fpreserve-control-flow -bargs -A=all.alis

    # Run and analyse all units except the test harness, filtering out
    # the correspond ALI files from the list:
    grep -v 'test_[^/]*.ali' all.alis > divmod0.alis
    gnatcov run --level=stmt+mcdc --scos=@divmod0.alis
    gnatcov coverage --level=stmt+mcdc --annotate=xcov --scos=@divmod0.alis


.. _passing_gpr:

Using project files (:cmd-option:`-P`, :cmd-option:`--projects`, :cmd-option:`--units`)
---------------------------------------------------------------------------------------

As an alternative to providing the complete list of coverage obligation files
with :cmd-option:`--scos` or :cmd-option:`--sid`, you can use project files to
specify units of interest. When both obligation files and project file options
are on the command line, the former prevail and the project files are only used
for switches or the determination of the target and runtime configuration.

The units of interest designation with project files incurs two levels of
selection: first, specify the set of :dfn:`projects of interest` where the
units of interest reside, then specify units of interest therein.

Conveying *projects* of interest
********************************

The set of projects of interest is computed by the following rules:

- A set of *base* projects is first selected from the recursive
  dependency closure of a root project;

- A set of *candidate* projects of interest is established, as the union of the
  dependency closures of all the base projects by default, or as the mere set
  of base projects alone if the :cmd-option:`--no-subprojects` switch is used;

- The actual projects of interest are the candidate ones minus those
  with an ``Externally_Built`` attribute set to ``"True"``.

For the determination of the base projects set, a single :dfn:`root project`
must first be specified using the :cmd-option:`-P` option. The set may then be
refined according to the following rules with an optional list of
:cmd-option:`--projects` switches naming projects within the dependency closure
of the root:

- Without :cmd-option:`--projects`, the base projects set is the root project
  designated by :cmd-option:`-P` alone, or the project designated by the
  ``Origin_Project`` attribute therein if there is such an attribute;

- With :cmd-option:`--projects` options, the listed projects are taken as the
  base and the root project needs to be listed as well to be included.

Let us illustrate the effect of various combinations, assuming an example
project tree depicted below:

.. image:: prjtree.*
  :align: center

Assuming none of the projects is flagged ``Externally_Built``:

- :ref:`fig-Proot-nosub` restricts the analysis to units in the root
  project only (:numref:`fig-Proot-nosub`);

- :ref:`fig-Proot-ss_a-nosub` focuses on Subsystem A alone
  (:numref:`fig-Proot-ss_a-nosub`);

- If the root project is also of interest, it must be listed
  explicitly, as in :ref:`fig-Proot-root-ss_a-nosub`
  (:numref:`fig-Proot-root-ss_a-nosub`);

- Removing :cmd-option:`--no-subprojects` as in :ref:`fig-Proot-ss_a`, lets you
  consider all the projects transitively imported by the base ones
  (:numref:`fig-Proot-ss_a`);

Projects with an ``Externally_Built`` attribute set to ``"True"`` are
just removed from the set of interest at the end, without influencing
the processing of dependency closures. In the last example above, if
project ``A1`` had the attribute set to ``"True"``, ``Common`` would
remain of interest to the assessment even though it was dragged as a
dependency of ``A1``.


.. _fig-Proot-nosub:
.. figure:: Proot-nosub.*
  :align: center

  ``-Proot --no-subprojects``

.. _fig-Proot-ss_a-nosub:
.. figure:: Proot-ss_a-nosub.*
  :align: center

  ``-Proot --projects=subsystem_a --no-subprojects``

.. _fig-Proot-root-ss_a-nosub:
.. figure:: Proot-root-ss_a-nosub.*
  :align: center

  ``-Proot --projects=root --projects=ss_a --no-subprojects``

.. _fig-Proot-ss_a:
.. figure:: Proot-ss_a.*
  :align: center

  ``-Proot --projects=subsystem_a``

Conveying *units* of interest within projects
*********************************************

By default, all the units encompassed by a project of interest are considered
of interest. This can be tailored first with specific attributes in package
``Coverage`` of project files.

Four attributes are available to control the set of units to be considered of
interest within a project: ``Units``, ``Units_List``, ``Excluded_Units``, and
``Excluded_Units_List``.

``Units`` and ``Units_List`` are used to construct an initial set of units for
which coverage analysis should be performed.  For example, given a project
with three packages ``Pak1``, ``Pak2``, and ``Pak3``, if you want to do
coverage analysis only for ``Pak1`` and ``Pak3`` you can specify::

  package Coverage is
    for Units use ("pak1", "pak3"); -- pak1 and pak3 are of interest
  end Coverage;

Similarily to ``Sources`` and ``Sources_List``, the ``Units`` attribute
specifies a set of units and ``Units_List`` specifies the name of a text file
containing a list of units.  See the :ref:`unit-names` section for details
how individual units should be denoted depending on the source language.

``Excluded_Units`` and ``Excluded_Units_List`` operate like ``Units`` and
``Units_List`` but for units that should never be considered of interest for
coverage. Back to our example, the same result as above is obtained by
specifying::

   package Coverage is
      for Excluded_Units use ("pak2");  -- all units except pak2 are of interest
   end Coverage;

When the exclude/include sets overlap, the excluding attributes prevail
over the including ones. The exact rules for computation of the units to be
considered of interest within a project are as follows:

- An initial set is determined using the ``Units`` and ``Units_List``
  attributes in the project's ``Coverage`` package; By default, if no such
  attribute is found, the initial set comprises all the units of the project,

- Units determined using the ``Excluded_Units`` and ``Excluded_Units_List``
  attributes are removed from the initial set to yield the set to consider.

Finally, the list of units of interest for a given execution of |gcv| can also
be overriden from the command line using the :cmd-option:`--units` switch.
When this option is used, the project files attributes are ignored.

The switch may appear multiple times. Each occurrence indicates one
unit to focus on, or with the @ syntax the name of a text file
containing a list of units to focus on, one per line. The effect of
multiple switches accumulate.

The effect of the example attributes provided previously could then
first be achieved with::

  gnatcov <command> -P... --units=pak1 --units=pak3

or by creating a ``units.list`` file with::

  pak1
  pak3

and then executing::

  gnatcov <command> --units=@units.list

.. _gpr_context:

Other switches or attributes of importance
******************************************

Independently from coverage considerations, project files offer a significant
range of possibilities to compose a software system.  To get an
accurate view of the set of projects and units from which those of interest to
coverage should be taken, |gcp| needs to operate on the same base information
as the builder.

To this effect, all the switches you would pass to gprbuild to control the
build configuration should also be passed to |gcp| commands when conveying
units of interest through project facilities, in particular ``--target``,
``--RTS``, ``--config`` switches very common in cross configurations, as well
as the ``-X`` series setting scenarii variables.

In some cases, such as ``--target`` or ``--RTS``, the effect of the command
line switch can be achieved with a project file attribute, which |gcp| knows
how to interepret as well.

.. _ignore_source_files:

Conveying source files to ignore / handling Ada subunits
--------------------------------------------------------

Two attributes in the ``Coverage`` package make it possible to specify
specific source file names for which the tool should not generate a
report eventually, even if the these sources are within units of
interest. This is intended for situations where the source files for a
unit of interest encompass some of its testing sources, for example
when parts of the testing code is implemented with ``separate``
subunits in Ada.

The dummy example below shows a possible organization of this kind,
with a ``Data_Processing`` package to be tested which contains a ``Test``
procedure declared as a ``separate`` entity::

  -- spec and body of a package to test

  package Data_Processing is
     procedure Process (X : Integer);

     procedure Test;
  end;

  package body Data_Processing is

     Internal_Data : Integer := 0;

     procedure Process (X : Integer) is
     begin
        ...
     end;

     procedure Test is separate; -- subunit declaration here
  end;

We can have different implementations of the ``Test`` subprogram body
in different source files and a project file based mechanism to select
one or the other based on a scenario variable::

  -- data_processing-test1.adb; test variation #1

  separate (Data_Processing)
  procedure Test is
  begin
     Process (X => 12);
     pragma Assert (Internal_Data > 0);
  end;

  -- data_processing-test2.adb; test variation #2

  separate (Data_Processing)
  procedure Test is
  begin
     Process (X => -8);
     pragma Assert (Internal_Data < 0);
  end;

  -- Project file with a Body source file name selection in a
  -- Naming project package:

  project P is
    TEST := external ("TEST");
    package Naming is
      for Body ("data_processing.test") use "data_processing-" & TEST & ".adb";
    end Naming;
  end P;

Then we can build one variant or the other with::

  -- run_all.adb

  with Data_Processing;
  procedure Run_All is
  begin
     Data_Processing.Test;
  end;

  $ gprbuild -Pp.gpr -XTEST=test1 run_all.adb
  $ gprbuild -Pp.gpr -XTEST=test2 run_all.adb
  ...

As any testing code, such subunits usually need to be excluded from
the coverage analysis scope. However, even though implemented in
separate source files, subunits are technically not units on their
own, so could not be excluded alone by the unit-based mechanisms
presented in previous sections.

The two attributes introduced here allow the specification of file
names to be ignored as a list of globbing patterns akin to those
allowed in Unix shells. All source files whose *base* name matches any
of the patterns are excluded from the analysis and from the output
report. Since only base names are matched, the provided patterns to
ignore should not include any path or directory component.

The first attribute, ``Ignored_Source_Files``, expects a direct list
of patterns. Even though intended for subunits, the attribute allows
file names corresponding to regular units as well. For our dummy
example, this could be::

    package Coverage is
      for Ignored_Source_Files use ("*-test*.adb", "run_all.adb");
    end Coverage;

The second one, ``Ignored_Source_Files_List``, expects the name of
a text file which contains the list of globbing patterns to ignore,
one line per pattern.

To achieve the same effect as with the first attribute for our
example, we could create a text file named ``ignore.list`` which would
contain::

  *-test.adb
  run_all.adb

And then have::

    package Coverage is
      for Ignored_Source_Files_List use "ignore.list";
    end Coverage;

As a possible alternative to the project file attributes, the |gcvcov| and
|gcvins| commands accept a :cmd-option:`--ignore-source-files` switch on the
command line.

This option can appear multiple times on the command line. Each
occurrence expects a single argument which is either a globbing
pattern for the name of source files to ignore (as for a
``Ignored_Source_Files`` attribute), or a :term:`@listfile argument`
that contains a list of such patterns (as for a
``Ignored_Source_Files_List`` attribute), and the effects of all the
options accumulate.

The example attributes provided previously would become::

  gnatcov <command> --ignore-source-files=*-test.adb --ignore-source-files=run_all.adb

or::

  gnatcov <command> --ignore-source-files=@ignore.list

When ``--ignore-source-files`` is provided on the command line, all
the ``Ignored_Source_Files`` and ``Ignored_Source_Files_List``
attributes are ignored.

.. _unit-names:

Compilation unit vs source file names
-------------------------------------

For Ada, explicit *compilation unit* names are given to library level packages
or subprograms, case insensitive. This is what must be used in project file
attributes or :cmd-option:`--units` arguments to elaborate the set of
:dfn:`units of interest`, not source file names.

This offers a simple and consistent naming basis to users, orthogonal to the
unit/source name mapping. Consider, for example, a project file with the set
of declarations below, which parameterizes the source file name to use for the
body of a ``Logger`` package depending on the kind of build performed::

  type Build_Mode_Type is ("Production", "Debug");
  Build_Mode : Build_Mode_Type := external ("BUILD_MODE", "Debug");

  package Naming is
     case Build_Mode is
        when "Production" =>
           for Implementation ("Logger") use "production-logger.adb";
        when "Debug" =>
           for Implementation ("Logger") use "debug-logger.adb";
     end case;
  end Naming;

Regardless of the build mode, restricting the analysis to the ``Logger``
package would be achieved with :cmd-option:`-P<project> --units=logger` or with
a ``Units`` attribute such as::

  package Coverage is
     for Units use ("Logger"); -- compilation unit name here
  end Coverage;


Source file names are used in the output reports, still, either in source
location references as part of the :cmd-option:`=report` outputs, or as the
base filename of annotated source files for other formats. For our ``Logger``
case above, the analysis with, for example, :cmd-option:`--annotate=xcov` of a
program built in Debug mode would yield a ``debug-logger.adb.xcov`` annotated
source result.

For C, the notion of *translation unit* resolves to the set of tokens that the
compiler gets to work on, after the pre-processing expansion of macros,
#include directives and the like. This doesn't have an explicit name and
:dfn:`units of interest` must be designated by the toplevel source file names
from which object files are produced.

Typically, from a sample ``foo.c`` source like:

.. code-block:: c

   #include "foo.h"

   static int bar (void)
   { ... }

   ...
   void foo (int x)
   { ... }


``gcc -c foo.c -fdump-scos ...`` would produce a ``foo.o`` object file, a
``foo.c.gli`` companion Library Information file, and excluding it from the
analysis scope can be achieved with::

  package Coverage is
     for Excluded_Units use ("foo.c"); /* source file name here  */
  end Coverage;
