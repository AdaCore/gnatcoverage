**Recognize statements in subprogram bodies and nested blocks therein**

Check statement coverage for statements in subprogram bodies and in
nested blocks contained in subprogram bodies.

Exercise several straight-line subprograms declared in a package.
Verify that statements in subprograms not called are reported as uncovered, and
that statements in subprograms that are called are not reported as uncovered.
The subprograms contain local blocks, so proper coverage of these is
verified as well.

