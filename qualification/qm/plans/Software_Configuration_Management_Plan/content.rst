======================================
Software Configuration Management Plan
======================================

The configuration management of qualification data is a responsibility of the applicant (see :qmref:`$(project)::Plans::Tool Qualification Plan::User_Activities`).

GNATcoverage is qualified as a verification tool. As such such most configuration management activities are not mandatory, in particular:


.. csv-table:: Compliance matrix for Table A-8
   :delim: |
   :header: "Item", "Description", "Ref.", "Notes"

   1|Configuration items are identified.|7.2.1|See list of qualification data in compliance matrix for 12.2.3 in :qmref:`$(project)::Plans::Tool Qualification Plan::Tool_Overview`.
   2|Baselines and traceability are established.|7.2.2|Not required for verification tools
   3|Problem reporting, change control, change review, and configuration status accounting are established.|7.2.3, 7.2.4, 7.2.5, 7.2.6|Not required for verification tools
   4|Archive, retrieval, and release are established.|7.2.7|Up to the applicant, see :qmref:`$(project)::Plans::Tool Qualification Plan::User_Activities`
   5|Software load control is established|7.2.8|Not required for verification tools
   6|Software life cycle environment control is established|7.2.9|Not required for verification tools

AdaCore internal configuration management process
=================================================

Configuration Management Methods
********************************

Configuration Management is technically implemented via a Subversion repository.
The life cycle of each artifact is automatically tracked in the repository. Informal email-based discussions about a precise artifact are also tracked. Each major artifact (requirement, qualification reports, etc.) is associated to a unique ID which is referenced in each email discussing the evolution of that precise artifact. In this manner, it is fairly easy to reconstruct the whole evolution of an artifact a posteriori simply by looking at SVN commits and email referencing its ID in their subjects. The mechanism used to implement this tracking uses the Customer Management System deployed at AdaCore: such technology has been widely used for the last ten years.

Official baseline production
----------------------------
Official baselines are generated on a customer-specific delivery for a precise operational environment. A specific folder and .zip file is created for each official release.

Archiving
---------
All repositories and mail servers are redounded with machines physically located in Paris (France) and New York (The United States). This increases our confidence on the durability of qualification data.

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
Open problems are tracked via emails. Each email is associated to a unique problem identified by a unique ID. Each problem is assigned to a single entity of the Development or Qualification team. The unique ID identify the open problem within a database which permits to track its evolution and status (open/closed). All emails are saved in a database and it is possible to query it to retrieve all mails related to a precise open problem.






