.. _adacore-configuration-items:

Version Controlled Items
************************

Development and Verification artifacts
--------------------------------------

All the GNATcoverage development and verification artifacts are version
controlled in AdaCore's configuration management system, in particular:

* All the TOR related artifacts (requirements, testcases, test sources, ...),
  each represented by a distinct file to permit atomic tracking,
* The GNATcoverage tool source code,
* The build/test infrastructure (framework to build the tool, run
  testsuites, monitor results, etc),

Plans and documentation
-----------------------

All the documentation artifacts and the framework to generate documents are
also under Configuration Management Control, with distinct files for different
sections or chapters of each document to permit fine grain automatic tracking.

Quality Assurance Reports
-------------------------

Quality Assurance Reports are atomically tracked exactly like any other
textual artifact of GNATcoverage qualification material. Quality assurance
reports are specific for each tool released and their lifecycle is tracked on
a release-specific basis.

Open Problems Identification
----------------------------

Open problems are tracked throughout their lifecycle. Each problem is associated
with a unique problem identifier, assigned when the issue is created.

At any given point in time, each issue is assigned to one or more members of the
Development or Qualification team, who are then responsible for the problem
management:

* launching investigations;
* developing corrections;
* updating the problem status (open/closed);
* or assigning to another team member for further actions until a resolution
  is found.

The complete problem evolution and status history are
tracked in a database thanks to the unique issue identifier.
