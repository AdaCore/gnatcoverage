# -*- coding: utf-8 -*-

"""
Check that if GNATcov is provided twice the same trace file, it consolidates as
expected an assembly-defined procedure assembled with -g (it has a compile
unit DIE, but no subprogram DIE in the DWARF info).
"""

import os.path
import shutil

from OCOV.tc import TestCase
from SUITE.context import env, thistest
from SUITE.cutils import Wdir
from SUITE.tutils import tracename_for

test_drivers = {
    'test': {'p.s': ['-g']},
}

coverage_expectations = {
    # There is only one "p" routine and we should consolidate them: we expect
    # only one "+" covered "p" routine.
    'p': {'-': 0, '!': 0, '+': 1},
}

tmp = Wdir('tmp_')

shutil.copy(os.path.join('..', 'test.c'), 'test.c')

# Select an assembly source that fits the build platform
variant = 'default'
if env.build.os.name == 'windows':
    variant = 'win32' if env.build.cpu.bits == 32 else 'win64'
shutil.copy(os.path.join('..', 'p-{}.s'.format(variant)), 'p.s')

TestCase(test_drivers, coverage_expectations,
         extra_xcov_args=[tracename_for('test')]).run()
thistest.result()
