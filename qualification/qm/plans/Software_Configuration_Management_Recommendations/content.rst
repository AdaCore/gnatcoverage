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

   #. The .zip package containing the .pdf files which consitute the GNATcoverage qualification kit, as defined in the :ref:`qualification-data` section of this document.
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

Items under management control
******************************

TOR related artifacts
---------------------

As the :ref:`qualification-data` section indicates, the TOR related artifacts
are organized as a filesystem tree.

Each individual *requirement* maps to a filesystem directory, holding a file
where the requirement text and testing strategy description reside.

Each *testcase* associated with a requirement maps to a distinct filesystem
subdirectory within the requirement directory subtree, holding a file which
contains the testcase description as well as the sources of tests used to
verify the tool behavior with respect to the testcase intent.

Intermediate *groups* of requirements or testcases are composed to provide a
structured organization, with an intermediate subdirectory assigned to each
and a file holding the group description text located therein.

Eventually, each individual artifact materializes as a distinct file in a tree
so as to permit atomic tracking.

Other development and verification artifacts
--------------------------------------------

* The GNATcoverage tool source code
* The build/test infrastructure (scripts and framework to build the tool, run
  testsuites, monitor nightly results ...)
* Scripts and framework to generate documentation (Qualifying Machine, ...)

Plans and documentation
-----------------------

All the documentation material is also under Configuration Management Control,
with distinct files for different sections or chapters of each document to
permit fine grain automatic tracking.

Quality Assurance Reports
-------------------------

Quality Assurance Reports are atomically tracked exactly like any other textual artifact of GNATcoverage qualification material. Quality assurance reports are specific for each tool released and their lifecycle is tracked on a release-specific basis.

Open problems identification
----------------------------

Open problems are tracked via emails. Each email is associated with a unique problem identified by a unique ID. Each problem is assigned with a single entity of the Development or Qualification team. The unique ID identifies the open problem within a database which permits its evolution and status (open/closed) to be tracked. All emails are saved in a database and it is possible to query it to retrieve all mails related to any open problem.

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

All the verification related artifacts for a specific customer/project
qualification are managed on a dedicated branch within our version control
systems. Qualification kits for the specific customer/project are produced off
that branch, which also tracks the QA cycles operated on the kits (QA reports,
corresponding corrections, ...).

Official baseline production
----------------------------

Official baselines are generated on a customer-specific delivery for a precise operational environment. A specific folder and .zip file is created for each official release. 

