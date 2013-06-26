SC expectations regarding ARM chap. 3 : Declarations and Types
==============================================================

SC expectations regarding ARM chap. 3 : Declarations and Types


Requirement(s)
--------------



Declarations that were never elaborated as part of the program execution shall
be reported as uncovered.


Testing Strategy
----------------



We exercise test programs for all the ARM chapter 3 elements of relevance from
a structural coverage analysis perspective:


.. qmlink:: TCIndexImporter

   *



For object declarations, we distinguish cases that might involve dynamic stack
or heap allocations as these require run-time library support that some
execution environments don't provide.

