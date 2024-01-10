Requirements for gnatcov command switches in project files
================================================================

The tool shall handle *Switches* attributes in the Coverage package of
the root project file, indexed by gnatcov command name and where the
value is a list of switch strings that could be passed to the gnatcov
command on the command line.

The following rules shall be obeyed:

======  ======================================================================
Rule #  Description
======  ======================================================================
1       There may be (at most) one such attribute per gnatcov command, plus an
        optional one for all the commands, designated by a '*' command name as
	the index.

2       An attribute for a specific command always overrides the '*'
        specification when there is one, regardless of the order in which they
	appear in the Coverage package.

3       For valued switches such as :option:`--level`, a value passed through
        an explicit switch on the real command line always takes precedence
	over one from the root project file.

4       When the scope of the gnatcov operation encompasses more than the root
        project, Switches attributes found in other projects are ignored.
======  ======================================================================

.. rubric:: Testing Strategy

We check the various aspects of the requirement with the following set
of testcases:

.. qmlink:: TCIndexImporter

   *

Rules #1 and #2 are checked together across variations around the
*coverage* and *run* commands, sometimes mixed with '*' specifications.

Rules #3 and #4 are tested separately, with a dedicated testcase for each.
