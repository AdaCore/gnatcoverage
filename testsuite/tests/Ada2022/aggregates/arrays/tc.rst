** DC for decisions nested in generalized array aggregates **

Check proper handling and decision coverage computing for decisions nested in
generalized array aggregates. Pkg implements a function presenting three forms
of generalized array aggregates, i.e. a positional empty aggregate, a single
element positional aggregate and a iterated component association aggregate.

The test drivers are articulated as follows:
 - Test_0 ensures only statement violations are reported when the
 function is not called,
 - Test_Empty ensures that decision coverage can be computed on a decision
 containing an empty generalized array aggregate,
 - Test_Single ensures that decisions nested within a single element positional
 aggregate are correctly processed,
 - Test_Multi ensures that decisions nested within iterated component
 associations in a generalized array aggregate are properly processed.
 - Test_Full ensures all the above can be covered simultaneously.
