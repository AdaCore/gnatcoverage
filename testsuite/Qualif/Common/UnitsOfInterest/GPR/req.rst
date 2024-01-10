Use of GPR project files to state Units Of Interest
===================================================

The tool shall support the use of project file facilities to
let users designate Units Of Interest for an analysis, as a replacement
of the mechanism where users would instead provide an explicit list
of files where the coverage obligations could be found.

The full set of rules is described in the *Specifying Units Of Interest*
chapter of the User Manual. To summarize here:

Command line switches :option:`-P`, :option:`--projects`, and
:option:`--no-subprojects` first select a set of *projects of interest*,
from which those flagged ``Externally_Built`` are pruned.

The tool operates in recursive mode by default, extended projects are allowed.

In absence of :option:`--units` switches, the *units of interest* is the union
of the units of interest attached to the projects of interest. For each
project, this is all the project's units by default, and this can be controled
with attributes in an optional ``Coverage`` package.
All of these unit specification options support globbing patterns.

:option:`--units` switches on the command line override that selection
entirely, allowing to specify a subset of actual interest.

.. rubric:: Testing Strategy

We check the various aspects of the requirement with the following set
of testcases:

.. qmlink:: TCIndexImporter

   *
