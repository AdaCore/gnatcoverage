import os
import os.path

from SCOV.minicheck import build_and_run, checked_xcov, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


wd = Wdir('tmp_')

# Generate a project for test drivers, build and run them to get trace files
prj_file = gprfor(['test_add.adb', 'test_mult.adb'],
                  srcdirs=os.path.join('..', 'src'),
                  extra='package Coverage is'
                        '\n  for Units use ("math");'
                        '\nend Coverage;')
xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=prj_file),
    covlevel='stmt+decision',
    mains=['test_add', 'test_mult'],
    extra_coverage_args=[])

# Extract trace files arguments from "xcov_args"
base_args = xcov_args[:-2]
add_trace = xcov_args[-2]
mult_trace = xcov_args[-1]

# Now, consolidate one trace file after the other, accumulating results in a
# single checkpoint (this is incremental coverage).
checked_xcov(base_args + ['--save-checkpoint=c1.ckpt', add_trace],
             'checkpoint-1.log')
checked_xcov(base_args + ['--checkpoint=c1.ckpt',
                          '--save-checkpoint=c2.ckpt', mult_trace],
             'checkpoint-2.log')

# Generate the final report and check we have the expected result
checked_xcov(base_args + ['--annotate=xcov', '--checkpoint=c2.ckpt',
                          '--output-dir=.'],
             'report.log')


# Finally, check we have the expected reports
check_xcov_reports(
    '*.xcov',
    {'math.adb.xcov': {'+': {7, 12, 13, 14, 18, 19}, '!': {17}},
     'math.ads.xcov': {}})

thistest.result()
