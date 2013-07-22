Recognize statements in subprogram bodies and nested blocks therein
====================================================================

Check that statements are recognized in subprogram bodies and nested blocks
therein.

Exercise several straight subprograms exposed by a package .
Verify that statements in subprograms not called are reported uncovered, and
that nothing is reported about statements in subprograms that are called. The
subprograms contain local block declarations so proper handling of these is
verified as well.

