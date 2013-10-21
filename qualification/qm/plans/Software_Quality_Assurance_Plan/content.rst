===============================
Software Quality Assurance Plan
===============================

Compliance to guidance
======================

GNATcoverage is qualified as a verification tool.

.. csv-table:: Compliance matrix for Table A-9 of DO-178B
   :delim: |
   :header: "Item", "Description", "Ref.", "Notes", "Activity"

   1|Assurance is obtained that software development and integral processes comply with approved software plans and standards.|8.1a|For verification tools, this is limited to the compliance of tool processes with approved plans|`Reading the PLANS document`_, `Inspection of qualification data`_
   2|Assurance is obtained that transition criteria for the software life cycle processes are satisfied.|8.1b|Not required for verification tools|not applicable
   3|Software conformity review is conducted.|8.1c, 8.3e, 8.3f| Items 8.3a, 8.3b. 8.3c, 8.3d, 8.3g, 8.3h and 8.3i are not required for verification tools or CC2; 8.3f is also not required because verification tool qualification is supposed to be black-box|`Tool Conformity Review`_

Quality Assurance Activities
============================

This section presents the Quality Assurance (QA) activities operated over the
documents composing a qualification kit in terms of objectives and
outputs.

The QA outputs for a kit are stored in a "qa" directory dedicated to the
project for which the kit is produced. Each QA cycle is assigned a
subdirectory there, named after the date at which the QA cycle started, in
*YYYYMMDD* format.

Reading the PLANS document
**************************

* **objectives:** 

  * Assess the compliance with qualification objectives.

* **output:** QA Reading report (qa/YYYYMMDD/qa_plans.doc)

Inspection of other qualification data
**************************************

Inspection of Tool Operational Requirements (by sampling)
---------------------------------------------------------


* **objectives:** 

  * Check the accuracy, completeness and consistency with respect to the qualification objectives.

* **output:** QA inspection report (qa/YYYYMMDD/qa_tor.doc).


Inspection of Test Cases (by sampling)
--------------------------------------


* **objectives:** 

  * Check the accuracy of test cases, in particular whether the tests exercise their target Ada constructs.

* **output:** QA inspection report (qa/YYYYMMDD/qa_tor.doc, the same file used for the review of Tool Operational Requirements).


Inspection of test execution results
------------------------------------


* **objectives:** 

  * Check the results of test execution.
  * In the case tests failed, it is necessary to investigate whether the source of error is:

    * A misbehavior of the infrastructure used to run tests and compare actual results to expected results: in this case, the GNATcoverage Qualification Team is in charge of reporting and fixing the problem.
    * A bug in the GNATcoverage implementation: in this case, the GNATcoverage Development Team is in charge of reporting and fixing the problem.
    * A reasonable limitation of the tool: in this case, the GNATcoverage Qualification Team is in charge of reporting and justifying the problem as part of the known limitations of the tool.

* **output:** QA inspection report (qa/YYYYMMDD/qa_str.doc)

Tool conformity review
**********************

The conformity review takes as input a packaged and qualifiable release of GNATcoverage.

* **objectives:** 

  * Record and approve software requirements deviations (8.3e).

* **output:** QA inspection report qa/YYYYMMDD/qa_conformity.doc)
