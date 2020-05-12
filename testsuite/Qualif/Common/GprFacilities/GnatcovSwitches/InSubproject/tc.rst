**Check that gnatcov switches in subprojects are ignored**

Build and run a test program, which volontarily performs stmt and
decision coverage violations while exercising applicative code units.

The applicative code is stored in a separate subdirectory, with a
local project file requesting stmt coverage only.

Enforce --level=stmt+decision and a report format in the root project,
request analysis on the recursive closure from that project and verify
that decisions coverage violations from the applicative code are
diagnosed as expected, in a report of the requested format.


