.. _adacore-configuration-items:

Version Controlled Items
************************

Development and Verification artifacts
--------------------------------------

All the GNATcoverage development and verification artifacts are version
controlled in our configuration management system, in particular:

* All the TOR related artifacts (requirements, testcases, test sources, ...),
  each materialized as a distinct file to permit atomic tracking,
* The GNATcoverage tool source code,
* The build/test infrastructure (framework to build the tool, run
  testsuites, monitor results ...),

Plans and documentation
-----------------------

All the documentation artefacts and the framework to generate documents are
also under Configuration Management Control, with distinct files for different
sections or chapters of each document to permit fine grain automatic tracking.

Quality Assurance Reports
-------------------------

Quality Assurance Reports are atomically tracked exactly like any other
textual artifact of GNATcoverage qualification material. Quality assurance
reports are specific for each tool released and their lifecycle is tracked on
a release-specific basis.

Open problems
-------------

Open problems are tracked via emails. Each email is associated with a unique
problem identifier, assigned by our tracking system when the first message
about an issue is received. At any given point in time, each issue is assigned
to a single member of the Development or Qualification team who is then
responsible for the problem management: launching investigations, developing
corrections, updating the problem status (open/closed) or assigning to
another team member for further action until a resolution is found. The
complete problem evolution (related email exchanges) and status history are
tracked in a database thanks to the unique issue identifier.

