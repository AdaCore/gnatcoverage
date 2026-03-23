"""
Regression test: gnatcov used to generated homonym buffer units for a
"Pkg.Child" unit and a "Pkg_Child" unit, as well as duplicate constant
declarations in some generated units, which prevented the build of the
instrumented sources.
"""

from SCOV.tc import TestCase
from SCOV.tctl import CAT
from SUITE.context import thistest


TestCase(category=CAT.stmt).run()
thistest.result()
