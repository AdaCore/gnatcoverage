"""
Check interactions of --units with -P / --projects.
"""

from SUITE.cutils import Wdir, list_to_tmp
from SUITE.context import thistest
from test_support import check


# Create a directory to hold temporary files, so that we do not accumulate
# uncleaned temporary files across testsuite runs.
temp_file_dir = Wdir('tmp_files')
temp_file_dir.to_homedir()

# Note that intops involves sub-units while boolops involves child-units,

# Lone unit, with sub units, in closure

check(
    root_project='gen.gpr',
    units=['intops'],
    recurse=True,
    xreports=['intops.ads', 'intops.adb',
              'intops-add.ads', 'intops-add.adb',
              'intops-sub.ads', 'intops-sub.adb'])

# Lone unit, with child units, in closure

check(
    root_project='../boolops/boolops',
    units=['boolops'],
    recurse=True,
    xreports=['boolops.ads', 'boolops.adb'])

# Likewise, using a response file to convey the units.

check(
    root_project='../boolops/boolops',
    units=['@%s' % list_to_tmp(['boolops'], dir='tmp_files')],
    recurse=True,
    xreports=['boolops.ads', 'boolops.adb'])

# Lone child unit from single project, either from -P + --projects
# or from a -P designating the subproject directly

check(
    root_project='../boolops/boolops',
    units=['boolops.andthen'],
    recurse=False,
    xreports=['boolops.ads', 'boolops-andthen.ads', 'boolops-andthen.adb'])

check(
    root_project='gen.gpr',
    projects=['boolops'],
    units=['boolops.andthen'],
    recurse=False,
    xreports=['boolops.ads', 'boolops-andthen.ads', 'boolops-andthen.adb'])

thistest.result()
