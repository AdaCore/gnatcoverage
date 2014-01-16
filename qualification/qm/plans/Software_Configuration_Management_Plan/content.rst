======================================
Software Configuration Management Plan
======================================

This section illustrates the Software Configuration process in place at AdaCore, the tool developer. 

GNATcoverage is a verification tool and, as such, its configuration items are managed at CC2.

.. csv-table:: Compliance matrix for Table A-8
   :delim: |
   :header: "Item", "Description", "Ref.", "Notes"

   1|Configuration items are identified.|7.2.1| 
   2|Baselines and traceability are established.|7.2.2|Only items 7.2.2f and 7.2.2g are required for CC2. 
   3|Problem reporting, change control, change review, and configuration status accounting are established.|7.2.3, 7.2.4, 7.2.5, 7.2.6|Only items 7.2.4a and 7.2.4b are required for CC2.
   4|Archive, retrieval, and release are established.|7.2.7|Only 7.2.7a, 7.2.7b(1) and 7.2.7e are required for CC2.
   5|Software load control is established|7.2.8|Not applicable to tools.
   6|Software life cycle environment control is established|7.2.9|7.2.9b apply to tools.


Configuration Management Activities
===================================

**Item 7.2.1:** The configuration items are listed in section :ref:`configuration-items`.
   
**Item 7.2.2f:** The following traceability relations are established:
 
 * From TOR to Test Cases: each TOR and Test Case is contained in a folder on the repository.
   Folders of Test Cases are nested inside the folders of the TOR they are tracebale
   to.

 Other trace data are not required for verification tools.

**Item 7.2.2g:** all configuration items are traceable to the GNATcoverage (pre-)qualification process.

**Items 7.2.4a, 7.2.4b and 7.2.7:** see :ref:`adacore-cm`

**Item 7.2.9b:** since GNATcoverage is a verification tool, the configuration management process should comply with Control Category 2.


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

.. _adacore-cm:

Configuration Management Methods and Activities
***********************************************

This section describes the internal configuration management process at
AdaCore. It is not related to the configuration management process deployed by the
applicant.

Base technical support
----------------------

Configuration Management of artifacts is technically implemented via a Git or
Subversion repository which tracks the life cycle of each artifact
automatically.  E-mail-based discussions about each artifact are also tracked,
using the AdaCore ticket system deployed within the whole company for more
than fifteen years now.

Archiving
---------

All repositories and mail servers are mirrored with machines physically located in Paris (France) and New York. This increases our confidence in the durability of qualification data.

Customer/Project specific tracking
----------------------------------

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
----------------------------

Official baselines are generated for customer-specific deliveries aimed at a
precise operational environment and software level. Once the configuration
management branch dedicated to such deliveries is set up, a complete testsuite
run is performed in the corresponding qualification environment. A kit version
number is then assigned and the qualification data documents are produced.
The resulting set of documents is packaged as a zip file which materializes
the kit as a whole. This kit then goes through QA as needed and all or part of
this cycle repeats until a positive agreement on the kit "acceptability" for
release is reached.

