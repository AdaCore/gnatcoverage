.. _trace-control:

**********************
Trace control for MCDC
**********************

MCDC analysis using execution traces requires specific care to make sure that
assessments are both accurate and efficient. In particular, branch history
collection (chronological record of the directions taken at conditional branch
points in the machine code) needs to be turned on for decisions that require
it, which is achieved by the combination of two indications passed to
|gcvrun|:

* :option:`--level=stmt+mcdc` to activate the collection of object branch
  histories,

* command line switches specifying what units will be subject to MCDC
  analysis, asking |gcv| to focus the branch history collections
  on the critical branches only, as identified by each unit's SCOs.
  This indication can be given either using project files, or using
  the low-level :option:`--scos` switch (see section :ref:`sunits`).

With :option:`--level=stmt+mcdc` and without any explicit selection of
relevant units, history is activated for all the object conditional branch
instructions, resulting in larger traces and increased processing time
compared to what is strictly needed. A few options are available to designate
the source units of interest, allowing optimal trace generation for more
efficient processing:

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

See :ref:`sunits` for extra information and examples describing their use.

Providing SCOs instructs |gcv| to restrict history collections to branches
that need it, allowing optimized operation downstream.  Care must be taken not
to request MCDC analysis for units whose SCOs were not included in the set
provided to |gcvrun|.

Statement or decision coverage assessments don't need trace history but are
not influenced by it. They can be performed with any kind of trace, including
history or not.

