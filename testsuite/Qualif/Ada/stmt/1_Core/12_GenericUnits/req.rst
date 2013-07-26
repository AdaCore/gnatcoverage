SC expectations for ARM chap. 12 : Generic Units
=================================================


.. rubric:: Requirement(s)



The code of a generic unit shall be reported as covered only if the generic is
instantiated, and the instance is either called (a generic subprogram
instantiation or a subprogram declared in an instance of a generic package)
or elaborated (a declaration or elaboration code in a generic package).
In all other cases the code from a generic unit shall be reported as uncovered.


.. rubric:: Testing Strategy



We check library-level and local generic declarations and instantiations
through the following set of testcases:


.. qmlink:: TCIndexImporter

   *



