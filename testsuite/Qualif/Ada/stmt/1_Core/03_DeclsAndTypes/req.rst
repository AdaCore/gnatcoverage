SC expectations for ARM chap. 3 : Declarations and Types
=========================================================


Requirement(s)
--------------



Declarations that were never elaborated as part of the program execution shall
be reported as uncovered.


Testing Strategy
----------------



Test programs are provided for all the ARM chapter 3 elements that are relevant
to structural coverage analysis:


.. qmlink:: TCIndexImporter

   *



For object declarations, we distinguish cases that might involve dynamic stack
or heap allocations, since these require run-time library support that
might not be provided by some execution environments.

