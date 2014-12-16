*********************
|gcvcov| command line
*********************

.. index::
   single: gnatcov coverage command line

.. _gnatcov_coverage-commandline:

Coverage analysis with |gcp| is performed by invoking |gcvcov| for a set of
critera queried via the :option:`--level` command line option. The general
interface synopsis is available from |gcv| :option:`--help`::

 gnatcov coverage OPTIONS TRACE_FILES

The available options are as follows:

:option:`-c`, :option:`--level` |marg| :
   Tell the set of coverage criteria to be assessed. The possible values are
   :option:`branch` and :option:`insn` for object coverage analysis, and
   :option:`stmt`, :option:`stmt+decision` and :option:`stmt+mcdc` for source
   coverage analysis.

:option:`-a`, :option:`--annotate` |marg| :
   Request a specific output report format.  All the criteria support
   ``xcov[+]``, ``html[+]``, ``dhtml`` and ``report`` formats, with
   interpretations that vary depending on the assessed criteria. See the
   corresponding sections in the Source and Object Coverage Analysis chapters
   of this documentation for more details.

:option:`-o` :
   Request that the synthetic report produced by ``--annotate=report`` be
   output in the provided filname instead of standard output by default. This
   is just ignored for other output formats.

.. _cov-outdir:

:option:`--output-dir` :
   Request that the report files (index and annotated sources for the
   ``xcov``, ``html`` and ``dhtml`` output formats) be output in the provided
   directory. If not specified, the default is the root project's object
   directory if using projects, and the current directory if not. The
   directory must exist prior to invoking |gcv|.

:option:`-t`, :option:`--target` :
  State the target architecture/board/abi for which the analyzed program was
  built.  This corresponds to the target prefix of your compilation toolchain,
  for example ``powerpc-elf`` or ``leon-elf``, and can also be specified as a
  ``Target`` attribute within the project file designated by -P, if any. By
  default, |gcv| assumes that this target is the same as the host
  environment. Stating the correct target is required for correct processing
  of project files.

:option:`-T`, :option:`--trace` |marg|, |rarg| :
   Provide the set of execution traces for which a report is to be
   produced. When multiple traces are provided, |gcv| produces a consolidated
   result, as if there had been a single execution producing one trace that
   would have been the catenation of all the individual traces.  See the
   :ref:`consolidation` section for a description of the consolidation
   facility.

:option:`--exec`:
   Override executable from traces. Trace files contain an indication of the
   executable used to generate them. This option causes the named executable
   to be loaded for coverage analysis, and to override the indication contained
   in any trace specified after it on the command line. An empty executable
   name may be specified to restore the default behaviour of using the
   indication contained in each trace file. Note that :option:`--exec` may
   appear last on the command line, in which case it applies to no trace file,
   but still causes the indicated executable to be included in the coverage
   analysis. This ensures that any code in that executable that is not exercised
   by some trace file will be reported as not covered.

:option:`--routines`, |rarg|:
   For object coverage analysis specifically, provide the list of object
   symbol names that correspond to routines for which the coverage assessment
   is to be performed. Each instance of this option on the command line adds
   to what is to be assessed eventually. See the :ref:`oroutines` section for
   extra details and use examples.

:option:`-P`:
   Use the indicated project file as the root project to select the units of
   interest for this analysis and find default options. Default options are
   taken only from this project. In absence of :option:`--recursive` and
   :option:`--projects`, the units of interest are those designated by this
   project only.

:option:`--non-coverable`:
   For source coverage analysis specifically, report about language
   statements for which no object code could be found in the surrounding
   suprogram (typically out of optimization).
   
:option:`--projects`, |rarg|:
   When using :option:`-P`, use the provided projects to select units of
   interest. These projects must all be part of the import transitive closure
   reachable from the root project designated by :option:`-P`.

:option:`--recursive`:      
   In addition to those designated by :option:`-P` / :option:`--projects`,
   consider units from any transtively imported project.

:option:`--units`, |rarg|:
   When using project files, override the list of units of interest for
   source coverage with those provided.

:option:`--subdirs`:
   When using project files, look for :term:`Library Information files` in the
   indicated subdirectory of each project's object directory.

:option:`--scos`, |rarg|:
   For source coverage analysis specifically, provide the set of
   :term:`Library Information files` from which Source Coverage Obligations
   (SCOs) should be loaded. This low-level switch effectively overrides the
   project based units of interest selection by the :option:`-P` family
   of options.

:option:`--alis`, |rarg|:
    Similar to :option:`--scos` in primary intent: provide set of
    :term:`Library Information files`. This is complementary to
    :option:`--scos` for operations that rely on library information
    items and don't require Source Coverage Obligations, in particular
    for gathering exemption regions applicable to object level criteria.

A lot of options are available to control the set of units for which coverage
is to be assessed. They may be combined in multiple ways and attributed within
the project files are available to refine the set of units to include or
exclude from each designated project. See :ref:`using-gpr` for a general
overview of how the project file facilities operate and :ref:`sunits` for
extra details and examples of use.

Elements on the command line that are not tied to a particular option are
considered as trace file arguments. At least one trace file is required for
the `coverage` command to operate, which may but need not be introduced with
:option:`-T` or :option:`--trace`. Here are a few examples of valid command
lines to illustrate. Other examples will be exposed along the course of the
following sections::

  gnatcov coverage --level=stmt --scos=@alis --annotate=report --trace=prog.trace
  #                      (a)         (b)              (c)            (d)
  # (a) Request Statement coverage assessment,
  # (b) for units associated with the ALI files listed in the "alis" text file,
  # (c) producing a synthetic text report on standard output (no -o option),
  # (d) out of a single execution trace "prog.trace".

  gnatcov coverage --level=stmt+decision --scos=@alis --annotate=html t1 t2
  # Statement and Decision coverage assessments for two traces "t1" and "t2",
  # producing html report files in the current directory.

  gnatcov coverage --level=stmt+decision --scos=@alis --annotate=html @mytraces
  # Same report, with t1 and t2 listed in the "mytraces" text file

  gnatcov coverage --level=stmt -Papp.gpr --annotate=html @mytraces
  # Same kind of report, focused on source units owned by the "app.gpr" only

  gnatcov coverage --level=stmt -Papp.gpr --recursive --annotate=html @mytraces
  # Likewise, considering all the projects transitively imported by app.gpr


