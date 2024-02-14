"""
Check that --output-dir maked gnatcov produce all annotated reports in the
output directory, none in the current directory.
"""

import os.path

from e3.fs import mkdir

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir, list_to_tmp, match
from SUITE.tutils import gprfor, xcov


Wdir('tmp_')


xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=gprfor(['main.adb'], srcdirs='..')),
    covlevel='stmt',
    mains=['main'],
    scos=['obj/main'],
    extra_coverage_args=['--level=stmt'])


# Check that the annotated reports are produced in the expected directory
mkdir('xcovs')
mkdir('htmls')
xcov(xcov_args + ['--annotate=xcov', '--output-dir=./xcovs'])
xcov(xcov_args + ['--annotate=html', '--output-dir=./htmls'])
xcov_report = 'main.adb.xcov'
html_report = 'index.html'
thistest.fail_if(os.path.exists('./' + xcov_report)
                 or os.path.exists('./' + html_report)
                 or not os.path.exists('./htmls/' + html_report)
                 or not os.path.exists('./xcovs/' + xcov_report),
                 'reports in the wrong output dir')

# Check that a proper error message is given when the output dir is not a
# directory.
p = xcov(xcov_args + ['--annotate=html', '--output-dir=./gen.gpr'],
         out='gen.gpr.out', register_failure=False)

thistest.fail_if(p.status == 0
                 or not match(': cannot create output path ./gen.gpr: ',
                              'gen.gpr.out'),
                 'no error if output dir is not a directory')

# Check that gnatcov can take a list of traces using the '@' prefix with the
# various forms to pass trace files on the command line, checking the output
# report in all cases.
trace_file = xcov_args.pop()
tlist = '@' + list_to_tmp([trace_file])
for arg in [
    ['--trace=' + tlist],
    ['-T', tlist],
    [tlist]
]:
    xcov(xcov_args + ['--annotate=xcov'] + arg)
    check_xcov_reports('.', {'main.adb.xcov': {'+': {3}}})

thistest.result()
