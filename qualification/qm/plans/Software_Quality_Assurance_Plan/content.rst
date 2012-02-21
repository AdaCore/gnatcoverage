===============================
Software Quality Assurance Plan
===============================

We remind here that GNATcoverage is qualified as a verification tool. As such, most configuration management activities are not mandatory, in particular:

.. csv-table:: Compliance matrix for Table A-9 of DO-178B
   :delim: |
   :header: "Item", "Description", "Ref.", "Notes", "Activity"

   1|Assurance is obtained that software development and integral processes comply with approved software plans and standards.|8.1a|For verification tools, this is limited to the compliance of tool processes with approved plans|`Reading of GNATcoverage Qualification Material: Plans`_, `Inspection of qualification data (by sampling)`_
   2|Assurance is obtained that transition criteria for the software life cycle processes are satisfied.|8.1b|Not required for verification tools|not applicable
   3|Software conformity review is conducted.|8.1c, 8.3e, 8.3f| Items 8.3a, 8.3b. 8.3c, 8.3d, 8.3g, 8.3h and 8.3i are not required for verification tools or CC2; 8.3f is also not required because verification tool qualification is supposed to be black-box|`Tool Conformity Review`_

Quality Assurance Activities
============================

This sections contains a description of the Quality Assurance activities in terms of objective and output.

Reading of GNATcoverage Qualification Material: Plans
*****************************************************

The quality assurance on plans is not specific to a precise client: reports are thus simply identified by date.

* **objectives:** to assess the compliance with qualification objectives
* **output:** QA Reading report (qa/DDMMYYYY/qa_plans.doc)

Inspection of qualification data (by sampling)
**********************************************

Inspection of Tool Operational Requirements (by sampling)
---------------------------------------------------------

The quality assurance on tool operational requirements is specific to a precise client and operational environment: reports are thus identified by the client name and a date.

* **objectives:** 

  * check the accuracy, completeness and consistency with plans.

* **output:** QA inspection report (qa/<CLIENT>/DDMMYYYY/qa_tor.doc).


Inspection of Test Cases (by sampling)
--------------------------------------

The quality assurance on tool operational requirements is specific to a precise client and operational environment: reports are thus identified by the client name and a date.

* **objectives:** 

  * check the accuracy of test cases, in particular the representativeness of target Ada constructs.

* **output:** QA inspection report (qa/<CLIENT>/DDMMYYYY/qa_tor.doc, the same file used for the review of Tool Operational Requirements).


Inspection of test execution results
------------------------------------

The quality assurance on tool operational requirements is specific to a precise client and operational environment: reports are thus identified by the client name and a date.

* **objectives:** 

  * Check the results of test execution
  * In the case tests failed, it is necessary to investigate whether the source of error is:

    * A misbehaviour of the infrastructure used to run tests and compare actual results to expected results: in this case, the GNATcoverage Qualification Team is in charge of reporting and fixing the problem.
    * A bug in the GNATcoverage implementation: in this case, the GNATcoverage Development Team is in charge of reporting and fixing the problem.
    * A reasonable limitation of the tool: in this case, the GNATcoverage Qualification Team is in charge of reporting and justifying the problem as part of the know limitations of the tool.

* **output:** QA inspection report (qa/<CLIENT>/DDMMYYYY/ qa_test_execution.doc)

Tool conformity review
**********************
The quality assurance on tool operational requirements is specific to a precise client and operational environment: reports are thus identified by the client name and a date. The conformity review takes in input a packaged and qualifiable release of GNATcoverage.

* **objectives:** 

  * Record and approve software requirements deviations (8.3e).

* **output:** QA inspection report qa/<CLIENT>/DDMMYYYY/ qa_conformity.doc)
