import os.path
import re
from shutil import copy

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of, set_from
from SUITE.tutils import (exepath_to, gprbuild, gprfor,
                          platform_specific_symbols, xcov)


Wdir('tmp_')

# Check that --exclude/--include options are properly supported.

gprbuild(gprfor(srcdirs=['../src'], mains=['p.adb']))
exe = exepath_to('p')

# Simple usage of disp-routines:

xcov(['disp-routines', exe, '--exclude', 'obj/b__p.o'],
     out='routines1.txt')

r1 = contents_of('routines1.txt')

for pattern in ('pack__q', 'pack__r', '_ada_p'):
    thistest.fail_if(not re.search(pattern, r1),
                     "'%s' not found in disp-routines output" % pattern)


# Check that "p.o - S" return the empty set if p.o is in S:

xcov(['disp-routines', '--include', 'obj/p.o', '--exclude', exe],
     out='routines2.txt')

thistest.fail_if(
    os.path.getsize('routines2.txt') > 0,
    'disp-routines -i p.o -o p.exe should return an empty set. Got:\n' +
    contents_of('routines2.txt'))

# Check various properties: that "S + p.o - p.o" is idempotent iff p.o is in S,
# that "p.o + b~p.o - p.o - b~p.o + p.o" do exclude b~p.o... comparing the
# output of disp-routines against a baseline (do not rely on the order of
# output symbols, it is not defined).

copy('obj/p.o', 'obj/p1.o')
copy('obj/p.o', 'obj/p2.o')
copy('obj/b__p.o', 'obj/b__p1.o')

xcov(['disp-routines', 'obj/p.o', 'obj/b__p.o', '--include', 'obj/pack.o',
      '--exclude', 'obj/p1.o', 'obj/b__p1.o', '--include', 'obj/p2.o'],
     out='routines3.txt')

baseline3 = set(platform_specific_symbols(set_from('../baseline3.txt')))
routines3 = set_from('routines3.txt')

thistest.fail_if(baseline3 != routines3,
                 'Advanced usage of disp-routines')

thistest.result()
