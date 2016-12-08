SC requirements for ARM chap. 12 : Generic Units
================================================


%REQ_ID%


For coverage assessments of generic subprograms or packages, the tool combines
the coverage achieved by different instantiations of the generic
entities. Statements or declarations within generic source code shall be
reported uncovered when they are never executed at all, regardless of the
instantiations from which the execution might originate.


.. rubric:: Testing Strategy



We check library-level and local generic declarations and instantiations
through the following set of testcases:


.. qmlink:: TCIndexImporter

   *



