**Check FCC for a call within an component selection**

Make a call to a subprogram returning a record type and access one of its
components directly.

gnatcov limitation: calls made in the prefix of a dotted name are currently
unable to be instrumented. Such calls should have their coverage reported as
undetermined.
