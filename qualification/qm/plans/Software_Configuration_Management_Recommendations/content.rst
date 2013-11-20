=================================================
Software Configuration Management Recommendations
=================================================

As the compliance matrix below points out and as the
:qmref:`/PLANS/Tool_Qualification_Plan/User_Activities` section of this
document synthesizes, the configuration management of qualification data is a
responsibility of the applicant. In this chapter we propose a possible
approach for the configuration management of the GNATcoverage qualification
data by the applicant and provide a high level overview of the internal
configuration management process established at AdaCore.

.. csv-table:: Compliance matrix for Table A-8
   :delim: |
   :header: "Item", "Description", "Ref.", "Notes"

   1|Configuration items are identified.|7.2.1| Up to the applicant.
   2|Baselines and traceability are established.|7.2.2|Only items 7.2.2f and 7.2.2g apply. Up to the applicant.
   3|Problem reporting, change control, change review, and configuration status accounting are established.|7.2.3, 7.2.4, 7.2.5, 7.2.6|Only items 7.2.4a nd 7.2.4b apply.  Up to the applicant.
   4|Archive, retrieval, and release are established.|7.2.7|Up to the applicant.
   5|Software load control is established|7.2.8|Not applicable.
   6|Software life cycle environment control is established|7.2.9|Only item 7.2.9b applies. Up to the applicant.


Configuration Management Recommendations
========================================

In this section we provide a set of recommendations for configuration management for the items that are the applicant's responsibility as per the compliance matrix above.

 * **Item 7.2.1:** The suggested configuration items are:

   #. The .zip packages corresponding to the data items described in the
      :ref:`qualification-data` section of this document.
   #. The GNAT Pro binary package, which contains the GNAT Pro executable toolset and the associated documentation, in particular the GNAT Pro User's Guide.
   #. The GNATemulator binary package, which contains the GNATemulator executable tool and the associated documentation.
   #. The GNATcoverage binary package, which contains the GNATcoverage executable tool and the associated documentation, in particular the GNATcoverage User's Guide.

 * **Item 7.2.2f:** we suggest establishing traceability between configuration items above by considering the tool executable names and versions.
 * **Item 7.2.2g:** we suggest tracing all configuration items to the GNATcoverage qualification process.
 * **Items 7.2.4a, 7.2.4b and 7.2.7:** with respect to these items, we suggest that the user deploy the most appropriate process depending on their own configuration management process and tools.
 * **Item 7.2.9b:** since GNATcoverage is a verification tool, the configuration management process should comply with Control Category 2.

.. _adacore-cm:

Overview of the AdaCore Configuration Management Process
========================================================

This section describes the internal configuration management process at
AdaCore and is provided for informational purposes only;
it is not related to the configuration management process deployed by the
applicant.

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

Configuration Management Methods and Activities
***********************************************

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

