package elaboration statements
===============================

Check SC for package elaboration

Check proper coverage assessment of statements executed during
the elaboration of both local and library-level packages.
Check that the only case where code from a package
body is reported uncovered is a local package declared in a subprogram body
that is not called.

