"""Test that the --exec-prefix option is implemented properly."""

import os
import os.path
import shutil

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import (exename_for, tracename_for, gprfor, gprbuild, xrun,
                          xcov)


# This test needs a clean temporary directory because already existing files
# from previous testsuite runs will disturb the executable lookup process.
if os.path.exists('tmp_'):
    shutil.rmtree('tmp_')
wd = Wdir('tmp_')

# Build the two set of sources as two projects. Each one has its own object
# directory (which is the exec directory as well).
for src_dir in ('prj1', 'prj2'):
    obj_dir = 'obj-{}'.format(src_dir)
    gprbuild(gprfor(srcdirs=[os.path.join('..', src_dir)],
                    objdir=obj_dir,
                    exedir=obj_dir,
                    mains=['foo.adb']))

exename = exename_for('foo')
tracename = tracename_for('foo')

prj1_exe = os.path.join('obj-prj1', exename)
prj2_exe = os.path.join('obj-prj2', exename)
doesnotexist_exe = os.path.join('doesnotexist', exename)

# Generate traces and coverage report for prj2's foo.adb while there is a "foo"
# executable in the current directory: make sure it works on prj2's program by
# making sure we find prj2 source chunks in Xcov reports.
shutil.copy(prj1_exe, exename)
xrun([prj2_exe])
xcov(['coverage', '--level=stmt', '--annotate=xcov',
      '--scos={}'.format(os.path.join('obj-prj2', 'foo.ali')),
      tracename])
thistest.fail_if(
    "This is prj2's foo.adb" not in contents_of('foo.adb.xcov'),
    '"gnatcov coverage" did not use the correct program'
)

# Now, when the provided path does not exists, make sure GNATcoverage does not
# find the executable in the current directory without the --exec-prefix
# option.
p = xrun([doesnotexist_exe], register_failure=False)
thistest.fail_if(
    p.status == 0,
    '"gnatcov run" found a program without --exec-prefix'
)

# And finally make sure it finds the executable with --exec-prefix
p = xrun([doesnotexist_exe, '--exec-prefix=obj-prj2'], register_failure=True)

thistest.result()
