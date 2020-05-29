Use of GPR project files to state Units Of Interest
===================================================

%REQ_ID%

The tool shall support the use of project file facilities to
let users designate Units Of Interest for an analysis, as a replacement
of the mechanism where users would instead provide an explicit list
of files where the coverage obligations could be found.

Command line switches :option:`-P`, :option:`--project`,
:option:`--no-subprojects`, and their associated optional Coverage package
define a first set of Units Of Interest, which :option:`--units`
switches may override (subset).

The tool operates in recursive mode by default, extended projects
are allowed.

.. rubric:: Testing Strategy

We check the various aspects of the requirement with the following set
of testcases:

.. qmlink:: TCIndexImporter

   *
