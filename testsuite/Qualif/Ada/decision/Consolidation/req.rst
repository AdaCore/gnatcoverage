DC expectations for combination of multiple execution traces
=============================================================

DC expectations regarding the combination of multiple
execution traces.


Requirement(s)
--------------



When the coverage achieved by multiple execution traces is evaluated, a
decision coverage violation shall only be reported when it would have been
reported for each trace individually.


Testing Strategy
----------------



We exercise consolidation of traces obtained for single test vector invocations
over a range of basic decisions.

For each decision, first run with every possible relevant input vector
independently, then check all the possible combinations of those input
vectors without repetitions.


.. qmlink:: TCIndexImporter

   *


