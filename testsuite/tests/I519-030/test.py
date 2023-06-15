"""
Check object coverage results in presence of inlining, with a single trivial
public routine called once from another unit.
"""

import re

from SUITE.context import thistest
from SUITE.control import target_info
from SUITE.cutils import Wdir, match
from SUITE.tutils import (exepath_to, gprbuild, gprfor, tracename_for, xcov,
                          xrun)


Wdir('tmp_')

# We expect the routine machine code to appear in two places: in the
# associated unit object and at the unique call site. We expect both to be
# attached to the routine source and only the latter to be exercised.
#
# With generics involved, similar expectations with multiple versions of the
# object code all attached to the unique generic source.
_target_info = target_info()

# Build with inlining activated. Don't force unreferenced symbols away.
gprbuild(project=gprfor(['test_kops.adb'], srcdirs='../src'),
         extracargs=['-O1', '-gnatn', '-save-temps'])

# Run, then check results.
xrun(exepath_to('test_kops'))

instances = ['ops4', 'ops8']

# We expect all the source level calls to have been inlined, hence the base
# object code alone never to be called
xcov(['disp-routines'] + ['obj/' + i + '.o' for i in instances],
     out='kops.list')
xcov(['coverage', '--level=branch', '--annotate=xcov+',
      '--routines=@kops.list', tracename_for('test_kops')])
thistest.fail_if(not match(r'-:.*X := X \+ K', 'kops.adb.xcov'),
                 'expect body of inlined not to be covered')

# And full object code coverage should report partial coverage only
xcov(['coverage', '--level=branch', '--annotate=xcov',
      tracename_for('test_kops')])
thistest.fail_if(not match(r'!:.*X := X \+ K', 'kops.adb.xcov'))

# Besides, attached to the generic source, we expect distinct object code
# sequences from the testing code (all covered, but we don't check for that).
# We request asm inserts for this purpose, and can't be sure about the exact
# layout of lines so have '.' match newlines as well.
xcov(['coverage', '--level=branch', '--annotate=xcov+',
      tracename_for('test_kops')])

symbol = _target_info.to_platform_specific_symbol('_ada_test_kops')
thistest.fail_if(
    not match('procedure Inc' + '.*'
              + '<{}.*'.format(symbol) * len(instances)
              + 'end Inc',
              'kops.adb.xcov', flags=re.DOTALL),
    'expect body of inlined attached to inlined source')

# Likewise, we expect one object code sequence for each generic instance (all
# uncovered, but we don't check for that). There's no guarantee on the order
# in which the instances object code will show up, so iterate on searches.
for i in instances:
    symbol = _target_info.to_platform_specific_symbol(i)
    thistest.fail_if(
        not match('procedure Inc' + '.*'
                  + '<' + symbol + '__inc'
                  + '.*' + 'end Inc',
                  'kops.adb.xcov', flags=re.DOTALL),
        'expect %s inlined machine code attached to inlined source' % symbol)

thistest.result()
