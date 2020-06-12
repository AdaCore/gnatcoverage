import re

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of, empty
from SUITE.tutils import exepath_to, gprbuild, gprfor, xcov, xrun
from SUITE.gprutils import Csw, gprcov_for


# Check correctness of transmission from Switches to commands for a few
# particular commands. Check that Switches ("*") comes before Switches (cmd).

wd = Wdir('wd_')


# We will be exercising combinations of run/coverage operations with option
# variations controlled via Coverage attributes in an otherwise common project
# file for a simple program.

def gprvariant(id, extra):
    return gprfor(prjid=id, srcdirs=['../src'], mains=['p.adb'], extra=extra)


exe = exepath_to('p')

# Build once
gprbuild(gprvariant(id='bld', extra=''))

# ------------------------------------------------
# -- Simple helpers for coverage/run variations --
# ------------------------------------------------
#
# --tag, valid only for gnatcov run.
# --annotate, valid only for gnatcov coverage.


def tag_for(id):
    return 'tag-%s' % id


def tagopt_for(id):
    return ['--tag=%s' % tag_for(id)]


def rep_for(id):
    return '%s.rep' % id


def annopt_for(id):
    return ['--annotate=report']


# level expected to be assessed eventually, always
lev = 'stmt+decision'


def levopt_for(id):
    return ['--level=%s' % lev]


def try_run(id, gpr):
    """gnatcov run with common set of options & variations via gpr."""
    log = id + '.rlog'
    xrun(['-P%s' % gpr, exe, '-o', '%s.trace' % id],
         out=log, register_failure=False)
    return contents_of(log)


def try_cov(id, gpr):
    """
    gnatcov coverage with common set of options & variations via gpr.  Expect
    valid options and check for commonly expected outcome
    """
    log = id + '.clog'
    xcov(['coverage', '-P%s' % gpr, '%s.trace' % id, '-o', rep_for(id)],
         out=log, register_failure=False)

    # Check that we get a report with expected contents wrt options
    # eventually. The tag & level checks make sure that
    #
    # * options intended for run do get there and not to coverage
    # * options intended for coverage do get there and not to run
    thistest.fail_if(not empty(log), 'unexpected contents in %s' % log)

    rep = contents_of(rep_for(id))
    thistest.fail_if('level: %s' % lev not in rep,
                     'missing expected level indication in %s' % rep_for(id))

    thistest.fail_if(not re.search('tag.*: %s' % tag_for(id), rep),
                     'missing expected tag indication in %s' % rep_for(id))


def check_valid_sequence_for(id, gprcov):
    """
    Common checking sequence for a specific gpr coverage package
    with valid options for both run and coverage.
    """
    gpr = gprvariant(id=id, extra=gprcov)
    try_run(id, gpr)
    try_cov(id, gpr)


# Basic check for starters. No "*".
id = 'basic'
check_valid_sequence_for(
    id=id, gprcov=gprcov_for(switches=[
        Csw('run', tagopt_for(id)),
        Csw('coverage', levopt_for(id) + annopt_for(id))
    ]))

# Check that "*" applies to all. Pass --level there.
id = 'star_valid'
check_valid_sequence_for(
    id=id, gprcov=gprcov_for(switches=[
        Csw('*', levopt_for(id)),
        Csw('run', tagopt_for(id)),
        Csw('coverage', annopt_for(id))
    ]))

# Check that command specific args prevail over "*", with "*" placed
# before in the gpr file.
id = 'star_postover'
check_valid_sequence_for(
    id=id, gprcov=gprcov_for(switches=[
        Csw('*', ['--level=stmt+mcdc']),
        Csw('run', tagopt_for(id)),
        Csw('coverage', levopt_for(id) + annopt_for(id))]))

# Likewise, with "*" placed after in the gpr file.
id = 'star_preover'
check_valid_sequence_for(
    id=id, gprcov=gprcov_for(switches=[
           Csw('run', tagopt_for(id)),
           Csw('coverage', levopt_for(id) + annopt_for(id)),
           Csw('*', ['--level=stmt+mcdc'])]))


# Check that "*" applies to all. Pass invalid for run, check failure.
id = 'star_invalid'
gpr = gprvariant(id=id, extra=gprcov_for(switches=[Csw('*', annopt_for(id))]))
rlog = try_run(id, gpr)

thistest.fail_if(
    '--annotate is not valid with the "run" command.' not in rlog,
    'missing expected failure indication in run log for %s' % id)

thistest.result()
