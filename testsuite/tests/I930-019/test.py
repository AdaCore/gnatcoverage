from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.cutils import Wdir, match
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


Wdir('tmp_')


# Run, then check results.
build_run_and_coverage(
    gprsw=GPRswitches(root_project=gprfor('test_kops.adb', srcdirs='../src')),
    covlevel='stmt',
    mains=['test_kops'],
    extra_coverage_args=['--annotate=xcov'],
    scos=['obj/kops', 'obj/ops4', 'obj/ops8', 'obj/test_kops', 'obj/vars'])

thistest.fail_if(not match(r'4 \+:', 'test_kops.adb.xcov'),
                 'test_kops.adb:4 not covered')
thistest.fail_if(not match(r'6 \+:', 'test_kops.adb.xcov'),
                 'test_kops.adb:6 not covered')
thistest.fail_if(not match(r'7 \+:', 'test_kops.adb.xcov'),
                 'test_kops.adb:7 not covered')
thistest.fail_if(not match(r'9 \+:', 'test_kops.adb.xcov'),
                 'test_kops.adb:9 not covered')
thistest.fail_if(not match(r'10 \+:', 'test_kops.adb.xcov'),
                 'test_kops.adb:10 not covered')
thistest.result()
