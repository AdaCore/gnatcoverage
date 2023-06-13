"""
Check that GNATcoverage knows about all pragmas listed in GNAT.
"""

from SUITE.context import thistest
from SUITE.tutils import xcov
from SUITE.cutils import Wdir, lines_of


wd = Wdir('tmp_')

# Get the list of pragmas that gnatcov knows
xcov(['dump-pragmas'], out='gnatcov.txt')
gnatcov_names = set(line.strip() for line in lines_of('gnatcov.txt'))

# Get the list of pragmas that GNAT knows
xcov(['dump-pragmas', '--gnat-pragmas'], out='gnat.txt')
gnat_names = set(line.strip() for line in lines_of('gnat.txt'))

# Check that gnatcov knows about all pragmas from gnat_util
missing_names = '\n'.join(sorted(gnat_names - gnatcov_names))
thistest.fail_if(
    missing_names,
    'gnatcov does not know about the following pragmas:\n' + missing_names)


thistest.result()
