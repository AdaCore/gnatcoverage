"""
This test checks that a line with empty instruction range at the beginning of a
function is not associated to the function that precedes it in the text
segment.
"""

import re

from SUITE.context import thistest
from SUITE.control import target_info
from SUITE.cutils import Wdir, match
from SUITE.tutils import (exepath_to, gprbuild, gprfor, tracename_for, xcov,
                          xrun)


Wdir('tmp_')
gprbuild(project=gprfor(['explore.adb'], srcdirs='../src'))

xrun(exepath_to('explore'))

xcov(['coverage', '--level=branch', '--annotate=xcov+',
      '--routines={}'.format(target_info().to_platform_specific_symbol(
          'robots__robot_control_inport'
      )),
      tracename_for('explore')])

thistest.fail_if(
    not match(r'\.:    function Robot_Situation_Outport',
              'robots.adb.xcov',
              flags=re.DOTALL),
    'expect line with empty instruction range dropped')

thistest.result()
