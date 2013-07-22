SC expectations for ARM chap. 12 : Generic Units
================================================


Requirement(s)
--------------



The code of a generic unit shall be reported as covered only if the generic is
instantiated, and the instantiation is either called (in case of a subprogram
instantiation or a subprogram declared in generic package) or elaborated (in
case of a declaration or elaboration code in a generic package). In all the
other cases the code from a generic unit shall be reported as uncovered.


Testing Strategy
----------------



We check libray-level and local generic declarations and instantiations
by way of the following set of testcases:


.. qmlink:: TCIndexImporter

   *



