======================================
Software Configuration Management Plan
======================================

This section illustrates the Software Configuration process in place at AdaCore, the tool developer. 

GNATcoverage is a verification tool and, as such, its configuration items are managed at CC2.

.. csv-table:: Compliance matrix for Table A-8
   :delim: |
   :header: "Item", "Ref.", "Achieved", "Notes"

   1|7.2.1|Yes|See :ref:`configuration-items`
   2|7.2.2a,b,c,d,e|No|Not required for CC2 (GNATcoverage is qualified as a verification tool)
   2|7.2.2f,g|Yes|See :ref:`traceability`
   3|7.2.3|No|Not required for CC2 (GNATcoverage is qualified as a verification tool)
   3|7.2.4a,b|Yes|See :ref:`adacore-change-control`
   3|7.2.4, 7.2.5, 7.2.6|No|Not required for CC2 (GNATcoverage is qualified as a verification tool)
   4|7.2.7a,b(1)|Yes|See :ref:`adacore-archive`
   4|7.2.7e|No|Not required for tools
   4|7.2.7b(2),(3),(4),c,d|No|Not required for CC2 (GNATcoverage is qualified as a verification tool)
   5|7.2.8|No|Does not apply to tools
   6|7.2.9a,c|No|Do not apply to tools
   6|7.2.9b|Yes|CC2

..  _configuration-items:

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
* Framework to generate documentation (Qualifying Machine, kit construction
  scripts...).

Plans and documentation
-----------------------

All the documentation material is also under Configuration Management Control,
with distinct files for different sections or chapters of each document to
permit fine grain automatic tracking.

Quality Assurance Reports
-------------------------

Quality Assurance Reports are atomically tracked exactly like any other textual artifact of GNATcoverage qualification material. Quality assurance reports are specific for each tool released and their lifecycle is tracked on a release-specific basis.

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

..  _traceability:

Traceability
************

**Item 7.2.2f:** The following traceability relations are established:
 
 * From TOR to Test Cases: each TOR and Test Case is contained in a folder on the repository.
   Folders of Test Cases are nested inside the folders of the TOR they are tracebale
   to.

 Other trace data are not required for verification tools.

**Item 7.2.2g:** all configuration items are traceable to the GNATcoverage (pre-)qualification process.

.. _adacore-cm:

Configuration Management Methods and Activities
***********************************************

This section describes the internal configuration management process at
AdaCore. It is not related to the configuration management process deployed by the
applicant.

Base technical support
----------------------

Configuration Management of artifacts is technically implemented via Git repositories 
which track the life cycle of each artifact automatically.  
E-mail-based discussions about each artifact are also tracked,
using the AdaCore ticket system deployed within the whole company for more
than fifteen years now.

The location of the GNATcoverage repository is:
**git+ssh://scm.forge.open-do.org/scmrepos/git/couverture**

The location of the GNATcoverege Qualification Kit repository is: 
**git+ssh://git.eu.adacore.com/scmrepos/git/gnatcoverage**

.. _adacore-change-control:

Change control
--------------

**Item 7.2.4a:** Integrity of configuration items is guaranteed by the Git
repositories where all configuration items are located. Only 
authorized engineers can modify configuration items and all modifications are
recorded. In addition, all repositories and mail servers are mirrored with 
machines physically located in Paris (France) and New York. This increases our confidence in the durability of qualification data.

**Item 7.2.4b:** Each change to a configuration item is associated to a unique
ID, which univocally identifies the version of a configuration item over its history.

.. _adacore-archive:

Archive, Retrieval and Release
------------------------------

**Item 7.2.7a:** Repositories are available for audit if necessary.

**Item 7.2.7b(1):** Only authorized engineers can change the configuration
items, thanks to the security mechanisms embedded in the Git
repositories.

Detailed procedures are as follows.

Customer/Project specific tracking
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For each specific customer/project qualification we assign an internal *kit
identifier*, referenced for example in QA reports. All the corresponding
verification related artifacts are managed on a dedicated *branch* within our
version control systems. Releases of qualification kits for the specific
customer/project are produced off that branch, which also tracks the QA cycles
performed on the kit items (QA reports, corresponding corrections, ...). A
typical kind of early change incorporated on the branch is the adjustment of
the targeted operational environment parameters, to be accounted for when
setting up the qualification environment for kit production cycles.


Official baseline production
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Official baselines are generated for customer-specific deliveries aimed at a
precise operational environment and software level. Once the configuration
management branch dedicated to such deliveries is set up, a complete testsuite
run is performed in the corresponding qualification environment. A kit version
number is then assigned and the qualification data documents are produced.
The resulting set of documents is packaged as a zip file which materializes
the kit as a whole. This kit then goes through QA as needed and all or part of
this cycle repeats until a positive agreement on the kit "acceptability" for
release is reached.

