=================================================
Software Configuration Management Recommendations
=================================================

The configuration management of qualification data is a responsibility of the applicant (see :qmref:`/PLANS/Tool_Qualification_Plan/User_Activities`). In this section we propose a possible approach for the configuration management of the GNATcoverage qualification data.

.. csv-table:: Compliance matrix for Table A-8
   :delim: |
   :header: "Item", "Description", "Ref.", "Notes"

   1|Configuration items are identified.|7.2.1| Up to the applicant.
   2|Baselines and traceability are established.|7.2.2|Only items 7.2.2f and 7.2.2g apply. Up to the applicant.
   3|Problem reporting, change control, change review, and configuration status accounting are established.|7.2.3, 7.2.4, 7.2.5, 7.2.6|Only items 7.2.4a nd 7.2.4b apply.  Up to the applicant.
   4|Archive, retrieval, and release are established.|7.2.7|Up to the applicant.
   5|Software load control is established|7.2.8|Not applicable.
   6|Software life cycle environment control is established|7.2.9|Only item 7.2.9b applies. Up to the applicant.


Configuration management recommendations
========================================

In this section we provide a set of recommendations for configuration management for the items that are the applicant's responsibility as per the compliance matrix above.

 * **Item 7.2.1:** The suggested configuration items are:

   #. The .zip package containing the .pdf files of the Plans (this document, plans.pdf), the Tool Operational Requirements and Test Cases and the test results.
   #. The GNAT Pro installer containing the tool executable object code, the GNAT Pro UG.
   #. The GNATemulator installer containing the tool executable object code.
   #. The GNATcoverage installer containing the tool executable object code and the GNATcoverage User's Guide.

 * **Item 7.2.2f:** we suggest establishing traceability between configuration items above by considering the tool executable names and versions.
 * **Item 7.2.2g:** we suggest tracing all configuration items to the GNATcoverage qualification process.
 * **Items 7.2.4a, 7.2.4b and 7.2.7:** with respect to these items, we suggest that the user deploy the most appropriate process depending on their own configuration management process and tools.
 * **Item 7.2.9b:** since GNATcoverage is a verification tool, the configuration management process should comply with Control Category 2.

Overview of the internal configuration management process at AdaCore
====================================================================

This section describes the internal configuration management process at
AdaCore and is provided for informational purposes only;
it is not related to the configuration management process deployed by the
applicant.

Configuration Management Methods
********************************

Configuration Management is technically implemented via a Subversion
repository or a Git repository.
The life cycle of each artifact is automatically tracked in the repository,
and e-mail-based discussions about each artifact are also tracked.
The mechanism used to implement this tracking uses the Customer Management
System deployed at AdaCore: such technology has been widely used for more
than ten years.

Official baseline production
----------------------------
Official baselines are generated on a customer-specific delivery for a precise operational environment. A specific folder and .zip file is created for each official release. 

Archiving
---------
All repositories and mail servers are mirrored with machines physically located in Paris (France) and New York. This increases our confidence in the durability of qualification data.

Activities
**********

Artifact identification
-----------------------
Each atomic artifact (single requirement, test case, expected output, compilation unit) is located in a single physical file, so as to permit its atomic tracking.

Plans and documentation
-----------------------

All documentation material is under Configuration Management Control. Each single part (each section) of each document is tracked automatically.

Development and Verification Artifacts
--------------------------------------

The tracked development and verification artifacts are:

* Source code
* Build/test infrastructure
* Tool Operational Requirements (TOR)
* Test Cases
* Tests
* Source code for tests
* Files containing the expected output
* Scripts to execute tests and generate documentation

Quality Assurance Reports
-------------------------

Quality Assurance Reports are atomically tracked exactly like any other textual artifact of GNATcoverage qualification material. Quality assurance reports are specific for each tool released and their lifecycle is tracked on a release-specific basis.

Open problems identification
----------------------------
Open problems are tracked via emails. Each email is associated with a unique problem identified by a unique ID. Each problem is assigned with a single entity of the Development or Qualification team. The unique ID identifies the open problem within a database which permits its evolution and status (open/closed) to be tracked. All emails are saved in a database and it is possible to query it to retrieve all mails related to any open problem.



