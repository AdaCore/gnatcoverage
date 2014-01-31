===============================
Software Quality Assurance Plan
===============================

Compliance With Guidance
========================

GNATcoverage is qualified as a verification tool.

.. csv-table:: Compliance matrix for Table A-9 of DO-178B
   :delim: |
   :header: "Item", "Ref.", "Achieved", "Notes"

   1|8.1a|Yes|See :ref:`reading-plans` and :ref:`inspecting-other-data`
   2|8.1b|Not applicable|GNATcoverage is a verification tool
   3|8.1c|Yes|See :ref:`tool-conformity-review`
   3|8.3a,b,c,d,e,f,g,h|Yes|See :ref:`tool-conformity-review`
   3|8.3i|Not applicable|No certification credit is sought for the use of previously developed software

Quality Assurance Activities
============================

This section presents the Quality Assurance (QA) activities performed on the
documents composing a qualification kit in terms of objectives and
outputs.

The QA outputs for a kit are stored in a "qa" directory dedicated to the
project for which the kit is produced. Each QA cycle is assigned a
subdirectory there, named after the date at which the QA cycle started, in
*YYYYMMDD* format.
The PDF format offers a sequential layout of the material which allows
browsing from end to end in a reliable manner, so most of the QA operations
described here are performed on the PDF version of the documents. Only those
steps requiring access to test sources are performed with the help of the HTML
contents.

.. _reading-plans:

Reading the PLANS document
**************************

* **objectives:** 

  * Assess the compliance with qualification objectives.

* **output:** QA Reading report (qa/YYYYMMDD/qa_plans.doc)

.. _inspecting-other-data:

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

  * Check that the Qualification Environment reported in the STR report
    is equivalent to the Operational Environment described in the TOR document.
  * Check the results of test execution.
  * In the case tests failed, it is necessary to investigate whether the source of error is:

    * A misbehavior of the infrastructure used to run tests and compare actual results to expected results: in this case, the GNATcoverage Qualification Team is in charge of reporting and fixing the problem.
    * A bug in the GNATcoverage implementation: in this case, the GNATcoverage Development Team is in charge of reporting and fixing the problem.
    * A reasonable limitation of the tool: in this case, the GNATcoverage Qualification Team is in charge of reporting and justifying the problem as part of the known limitations of the tool.

* **output:** QA inspection report (qa/YYYYMMDD/qa_str.doc)

.. _tool-conformity-review:

Tool conformity review
**********************

These objectives are achieved in the scope of the tool conformity review:

 * **8.3a**: Check that activities of the Tool Qualification Plan, the Software Configuration Management Plan 
   and Software Quality Assurance Plan are completed and have produced the items described 
   at :qmref:`../Software_Configuration_Management_Plan`
 * **8.3b**: Check that testcases and test results are traceable to Tool Operational Requirements
 * **8.3c**: Check that software life cycle data is under configuration management and managed as described
   in :qmref:`../Software_Configuration_Management_Plan`
 * **8.3d**: Check that problems have been reported and evaluated as per the :qmref:`/PLANS/Software_Configuration_Management_Plan`.
 * **8.3e**: Record and approve software requirements deviations exposed by test failures reported in the GNATcoverage Software Test Results report provided as part of the tool qualification data.
 * **8.3f**: Check that the tool executable object code can be re-generated from the tool source code.
 * **8.3g**: Check that the tool executable can be loaded and executed on the qualification environment.
 * **8.3h**: Check that problems from a previous Tool Conformity Review are re-evaluated.

**output:** QA inspection report (qa/YYYYMMDD/qa_conformity.doc)
