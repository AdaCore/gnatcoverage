"""
Check that a we can run the engines example with all possible coverage
level/annotations without crashing. And check the xcov/xcov+ annotated reports.
"""

from SUITE.context import thistest
from SUITE.control import target_info
from SUITE.cutils import FatalError, Wdir, match
from SUITE.tutils import (exepath_to, gprbuild, gprfor, tracename_for, xcov,
                          xrun)


Wdir('tmp_')
gprbuild(project=gprfor(['test_engines.adb'], srcdirs='../src'))
xrun(exepath_to('test_engines'))
line = ':       return E.P < Stable_P and then E.T < Stable_T;'


def src_cov(cov_level, annotate_level, line_coverage):
    try:
        xcov(['coverage',
              '--level=' + cov_level, '--annotate=' + annotate_level,
              '--scos=obj/engines.ali', tracename_for('test_engines')])
        if annotate_level.find('xcov') != -1:
            thistest.fail_if(not match('[0-9]+ ' + line_coverage + line,
                                       'engines.adb.xcov'),
                             'check ' + cov_level +
                             ' coverage results on engines example')
    except FatalError:
        pass


def obj_cov(cov_level, annotate_level, line_coverage):
    symbol = target_info().to_platform_specific_symbol('engines__stable')
    xcov(['coverage',
          '--level=' + cov_level, '--annotate=' + annotate_level,
          '--routines={}'.format(symbol), tracename_for('test_engines')])
    if annotate_level.find('xcov') != -1:
        thistest.fail_if(not match('[0-9]+ ' + line_coverage + line,
                                   'engines.adb.xcov'),
                         'check ' + cov_level +
                         ' coverage results on engines example')


src_cov_levels = ['stmt', 'stmt+decision', 'stmt+mcdc']
annotate_levels = ['xcov', 'html']

obj_cov_levels = ['insn', 'branch']
obj_annotate_levels = annotate_levels + ['asm', 'html+', 'xcov+']

line_coverage = {'insn': r'\+',
                 'branch': '!',
                 'stmt': r'\+',
                 'stmt+decision': r'\+',
                 'stmt+mcdc': '!'}

for cov_level in obj_cov_levels:
    for annotate_level in obj_annotate_levels:
        obj_cov(cov_level, annotate_level, line_coverage[cov_level])

for cov_level in src_cov_levels:
    for annotate_level in annotate_levels:
        src_cov(cov_level, annotate_level, line_coverage[cov_level])

thistest.result()
