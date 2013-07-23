Recognize statements in package elaboration bodies and nested blocks therein
=============================================================================

Check statement coverage for statements in package bodies and
in nested blocks inside such bodies.

"with" a package known to only contain straight elaboration code, and verify
that nothing is reported uncovered for this body. Include a local
block in that package body, to check that nested statements are correctly
reported as covered.


