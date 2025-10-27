**Test FCC for calls made as default values**

Check that the coverage of a call made to set a record component's default
value has correct is correctly reported.

gnatcov limitation: also check that a call made to set a subprogram parameter's
default value had no associated coverage information. No source coverage
obligation should be emitted for such calls.
