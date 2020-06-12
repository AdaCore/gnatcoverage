import re

from gnatpython.fileutils import mkdir

from SCOV.minicheck import build_and_run
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import Csw, GPRswitches, gprcov_for
from SUITE.tutils import gprfor, xcov


pgm = 'test_lt0'
wd = Wdir('wd_', clean=True)

gpr = gprfor(mains=[pgm + '.adb'],
             srcdirs=['../src'],
             extra=gprcov_for(switches=[
                Csw('*', ['--level=stmt']),
                Csw('coverage', ['--annotate=report'])]))


def run(extra_args, covlevel=None):
    """
    Build and run the single test program, which volontarily performs stmt and
    decision coverage violations.
    """
    xcov_args = build_and_run(
        gprsw=GPRswitches(root_project=gpr),
        covlevel=covlevel, mains=[pgm],
        extra_coverage_args=[])
    xcov(xcov_args + extra_args)


def check_report(label, filename, pattern, check_present=True):
    report = contents_of(filename)
    matched = re.search(pattern, report)
    thistest.fail_if(not matched if check_present else matched,
                     label)


# Check that we get results corresponding to the project file
# defaults if we don't tell anything otherwise.

run(['-o', 'def.rep'])

# Check that we have the output report, that it does contain at least a stmt
# coverage violation note, and that it doesn't contain any DC related note.
check_report('missing expected stmt coverage failure indication',
             'def.rep', 'statement not executed')
check_report('unexpected decision coverage failure indication',
             'def.rep', 'decision outcome .* never exercised',
             check_present=False)

# Override --level up to DC. Check that we now find the DC violations we
# expect.

run(['-o', 'lev.rep'], covlevel='stmt+decision')
check_report('missing expected decision coverage failure indication',
             'lev.rep', 'decision outcome .* never exercised')

# Override --annotate only. Expect full coverage on the "if"
# statement.

mkdir('sc')
run(['--annotate=xcov', '--output-dir=sc'])
check_report('missing expected full coverage indication',
             'sc/values.adb.xcov', r'\+:.*if')

# Override --annotate and --level. Expect partial coverage on the "if"
# statement.

mkdir('dc')
run(['--annotate=xcov', '--output-dir=dc'], covlevel='stmt+decision')
check_report('missing expected partial decision coverage indication',
             'dc/values.adb.xcov', '!:.*if')

thistest.result()
