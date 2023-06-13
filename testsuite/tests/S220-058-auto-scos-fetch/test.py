"""
Test that "gnatcov coverage" does not fetch ALI/SID files when only loading a
checkpoint (i.e. not processing a trace file).
"""

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir, contents_of, indent
from SUITE.tutils import gprfor, xcov


wd = Wdir('tmp_')

# Generate a checkpoint
ckpt = 'run0.ckpt'
gpr = gprfor(['main.adb'], srcdirs='..')
build_run_and_coverage(
    gprsw=GPRswitches(root_project=gpr),
    covlevel='stmt',
    mains=['main'],
    extra_coverage_args=['--save-checkpoint', ckpt],
    scos=['obj/main'])

# Override ALI/SID files with invalid content, so that attempts to load them
# result in errors.
for f in ['obj/main.sid', 'obj/main.ali']:
    with open(f, 'w') as f:
        f.write('INVALID FILE\n')

# Use the checkpoint to produce a report. Since there is no trace file to
# process, this should not try to load any ALI/SID file even though the loaded
# project makes them available.
log = 'coverage-ckpt.log'
xcov(['coverage', '--level=stmt', '--annotate=xcov',
      '--checkpoint', ckpt,
      '-P', gpr,
      '--output-dir=.'],
     out=log)
log_content = contents_of(log)
thistest.fail_if(log_content,
                 'Output of gnatcov not empty:\n{}'
                 .format(indent(log_content)))

thistest.result()
