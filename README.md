GNATcoverage
============

GNATcoverage is a tool to analyze and report program coverage.

Originally developed as part of the Couverture research project, GNATcoverage
allows coverage analysis of both object code (instruction and branch coverage),
and Ada or C language source code (Statement, Decision and Modified
Condition/Decision Coverage - MC/DC).

Please refer to the dedicated README files for [build
instructions](tools/gnatcov/) and for [testing](testsuite/).


Documentation
-------------

The [GNATcoverage User manual](http://docs.adacore.com/gnatcoverage-docs/html/gnatcov.html)
is a Sphinx documentation and is tracked in [this repository](tools/gnatcov/doc/umanual).
In order to build the HTML view, install Sphinx and run the following command
from the `tools/gnatcov/` directory:

    make doc.html


License
-------

GNATcoverage is licensed under the terms of the GPLv3 (General Public License
version 3). See [LICENSE](LICENSE) for more information.
