**Check FCC for a call to a tagged type primitive in different contexts**

Make a call to a subprogram returning a tagged record and access a field of
the return value.

gnatcov limitation: calls in the prefix of dotted names cannot be instrumented,
their coverage is reported as undetermined.

gnatcov limitation: gnatcov currently cannot instrument and report coverage
for expression functions that are primitives of tagged types.
