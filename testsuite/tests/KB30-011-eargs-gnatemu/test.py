import os.path
import re

from e3.os.process import Run

from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir, contents_of, to_list
from SUITE.tutils import gprbuild, gprfor, tracename_for, unixpath_to, xrun


Wdir('tmp_')

pgm = 'myinc'
unit = pgm + '.adb'
exe = unixpath_to(pgm)
trace = tracename_for(pgm)

# "gnatcov run" normalizes the name of the output trace file before spawning
# the low-level trace producer. Because GNAT's filename normalization is
# different from Python's, we cannot guess here what is the extract trace file
# name that will appear in logs. Workaround this by only try to match the
# basename.
trace_basename = os.path.basename(trace)

gprbuild(gprfor(mains=[unit], srcdirs=['..']))


def check_eargs(output, eargs, exe, protect_eargs=False):
    m = re.search('exec:\n  .*gnatemu.*', output)
    e = m.group(0)
    if protect_eargs:
        eargs = ' '.join ([f"'{arg}'" for arg in eargs.split(" ")])
    thistest.fail_if(
        not re.search("'--eargs' .*%(eargs)s.* '--eargs-end'.*%(exe)s"
                      % {'eargs': eargs, 'exe': exe}, e),
        "failed to match eargs='%s' and exe='%s' in output: <<\n%s\n>>"
        % (eargs, exe, output))


def gnatcovrun(extra, exe, register_failure=False, eargs=None):
    outfile = 'gnatcov.out'
    xrun('-v %(extra)s %(exe)s %(eargs)s'
         % {'extra': extra, 'exe': exe,
            'eargs': ('-eargs %s' % eargs) if eargs else ''},
         out=outfile, register_failure=register_failure)
    return contents_of(outfile)


def gnatemulator(args, exe):
    outfile = 'gnatemu.out'
    Run(to_list('%(target)s-gnatemu %(args)s %(kernel)s %(exe)s'
                % {'target': env.target.triplet,
                   'args': args,
                   'kernel': (('--kernel=' + thistest.options.kernel)
                              if thistest.options.kernel else ''),
                   'exe': exe}),
        output=outfile)
    return contents_of(outfile)


# stmt coverage, base check on exe expansion + no history request for traces
# absence of eargs
extra = '--level=stmt'
thistest.log(extra)
output = gnatcovrun(extra=extra, exe=exe)
check_eargs(output, eargs=trace, exe=exe)

# Check absence of "history" trace request for decision coverage
extra = '--level=stmt+decision'
thistest.log(extra)
output = gnatcovrun(extra=extra, exe=exe)
check_eargs(output, eargs=trace, exe=exe)

# Check "history" trace request + warning for mcdc coverage without scos.
# This should activate the history for all branches and might overrun the
# trace size limits in some configurations. In particular VxWorks, where we
# run the entire OS, and targets where we might encounter busy loops in the
# runtime for cache management.
extra = '--level=stmt+mcdc'
thistest.log(extra)
is_ignored_target = any(t in env.target.triplet for t in ['vx', 'aarch64'])
output = gnatcovrun(register_failure=not is_ignored_target,
                    extra=extra, exe=exe)
check_eargs(output, eargs='history,[^,]*%s' % trace_basename, exe=exe)
thistest.fail_if(
    not re.search('warning: No SCOs specified for MC/DC level', output),
    'failed to match no-sco warning')

# Check "history" trace request with map + absence of warning. The decision map
# filename is random, so use a wide matcher (anything but a comma) for it.
extra = '--level=stmt+mcdc --scos=obj/%s.ali' % pgm
thistest.log(extra)
output = gnatcovrun(extra=extra, exe=exe)
check_eargs(output, eargs='histmap=[^,]+,[^,]*%s' % trace_basename, exe=exe)

# Basic call with -eargs, check that they are included in the (single)
# --eargs group as well, and check that qemu sees them
eargs = '-version -m 512'
extra = '--level=stmt'
thistest.log(extra + ' eargs for qemu')
output = gnatcovrun(extra=extra, exe=exe, eargs=eargs)
check_eargs(output, eargs=eargs, exe=exe, protect_eargs=True)

eargs1 = '-m 512'
eargs2 = '-version'
output = gnatemulator('-v --eargs %s --eargs-end --eargs %s --eargs-end'
                      % (eargs1, eargs2), exe)
thistest.fail_if(
    not re.search('exec:.*qemu.*%s.*%s' % (eargs1, eargs2), output),
    'failed to match both --eargs sequences')

thistest.result()
