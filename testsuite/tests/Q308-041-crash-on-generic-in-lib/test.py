"""
Check that "gnatcov coverage" does not crash when processing a program that
exposes two paths for the same source (once before and one after installation).

Note that in this testcase, one absolute path comes from the debug info line
mapping (.debug_lines) while the other only comes from the debug info
DW_TAG_compilation_unit entry (.debug_info).
"""

import os

from e3.fs import rm
from e3.os.process import Run

from SUITE.tutils import thistest


def try_run(cmd):
    p = Run(cmd)
    thistest.fail_if(
        p.status != 0,
        'Unexpected failure.\n'
        'Command was:\n'
        '%s\n'
        'Output was:\n'
        '%s' % (' '.join(cmd), p.out))


home = os.getcwd()

os.chdir('%s/libfoo' % home)
rm('install', recursive=True)

try_run(['gprbuild', '-f', '-Plibfoo.gpr', '-p'])
try_run(['gprinstall', '-f', '-Plibfoo.gpr', '-p',
         '--prefix=install', '--project-subdir=gpr'])

os.chdir('%s/app' % home)

try_run(['gprbuild', '-f', '-Pdefault.gpr', '-p'])
try_run(['gnatcov', 'run', 'obj/main'])

# The very goal of this testcase is to compute code coverage for a unit that
# belongs to a project installed with gprinstall, so we need to enable the
# processing of externally built projects.
try_run(['gnatcov', 'coverage',
         '--annotate=xcov',
         '--level=stmt',
         '-Pdefault', '--projects=libfoo', '--externally-built-projects',
         'main.trace'])
thistest.result()
