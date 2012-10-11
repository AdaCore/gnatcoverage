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
   Generate coverage report
   -c LEVEL --level=LEVEL      Specify coverage levels
      LEVEL is one of branch|insn|stmt|stmt+decision|stmt+mcdc|stmt+uc_mcdc
   -a FORM  --annotate=FORM    Generate a FORM report
      FORM is one of asm,xcov,html,xcov+,html+,report
   --routines=<ROUTINE|@FILE>  Add ROUTINE, or all routine listed
                               in FILE to the list of routines
   -P<ROOT-PROJECT>            Use the indicated root project
   --projects=<PROJECT|@LISTFILE>
                               Consider the SCOs for units of interest in
                               PROJECT, or each project listed in LISTFILE
                               (requires -P)
   --recursive                 Also consider units from any imported project
   --units=<UNIT|@LISTFILE>    Consider the SCOs for the indicated UNIT, or
                               each unit listed in LISTFILE (requires -P)
   --subdirs=<SUBDIR>          Look for ALI files in the given SUBDIR of
                               projects' build directory (when using -P)
   --scos=<FILE|@LISTFILE>     Consider all the SCOs in ALI file
                               FILE for this operation; or do that
                               for each file listed in LISTFILE
   --output-dir=DIR            Put the =html|xcov outputs into DIR
   -o FILE                     Put the =report output into FILE
   -T|--trace <FILE|@LISTFILE> Add FILE or all the files listed in
                               LISTFILE to the list of traces

:option:`-c`, :option:`--level` |marg| :
   Tell the set of coverage criteria to be assessed. The possible values
   are split in two categories, one for source coverage and one for
   object coverage.

:option:`-a`, :option:`--annotate` |marg| :
   Request a specific output report format.  All the criteria support
   ``xcov[+]``, ``html[+]`` and ``report`` formats, with interpretations
   that vary depending on the assessed criteria. See the corresponding
   sections in the Source and Object Coverage Analysis chapters of this
   documentation for more details.

:option:`-o` :
   Request that the synthetic report produced by ``--annotate=report`` be
   output in the provided filname instead of standard output by default. This
   is just ignored for other output formats.

.. _cov-outdir:

:option:`--output-dir` :
   Request that the report files (index and annotated sources for the ``xcov``
   and ``html`` output formats) be output in the provided directory. They are
   output in the current directory, where |gcv|, is launched, otherwise. The
   directory must exist prior to invoking |gcv|.

:option:`-T`, :option:`--trace` |marg|, |rarg| :
   Provide the set of execution traces for which a report is to be
   produced. When multiple traces are provided, |gcv| produces a consolidated
   result, as if there had been a single execution producing one trace that
   would have been the catenation of all the individual traces.  See the
   :ref:`consolidation` section for a description of the consolidation
   facility.

:option:`--routines`, |rarg|:
   For object coverage analysis specifically, provide the list of object
   symbol names that correspond to routines for which the coverage assessment
   is to be performed. Each instance of this option on the command line adds
   to what is to be assessed eventually. See the :ref:`oroutines` section for
   extra details and use examples.

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
   switches :option:`--projects` and :option:`--units`.

See section :ref:`sunits` for extra details and use examples about the
various switches used to specify units of interest for source coverage.

Elements on the command line that are not tied to a particular option are
considered as trace file arguments. :option:`--trace` is marked mandatory only
to indicate that at least one trace file is required, which may but need not
be introduced with :option:`-T` or :option:`--trace`. Here are a few examples
of valid command lines::

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

