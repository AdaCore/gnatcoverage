"""
We check that gnatcov can take a list of ALI/SID files using the '@' prefix or
a single input file.
"""

from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, list_to_tmp
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, xcov


wd = Wdir('tmp_')
trace_file = build_and_run(
    gprsw=GPRswitches(root_project=gprfor(srcdirs=['../src'],
                                          mains=['trymon.adb'])),
    covlevel='stmt',
    mains=['trymon'],
    extra_coverage_args=[])[-1]

coverage_base_args = ['coverage', '--level=stmt', '--annotate=xcov',
                      trace_file]
expected_reports = {
    'trymon.adb.xcov': {'+': {24}},
    'monitor.adb.xcov': {'+': {4}},
    'monitor.ads.xcov': {},
}
# Unlike in binary trace mode, where it is no-code, instrumentation allows to
# detect that the Integer library-level variable declaration at line 2 is
# covered.
if thistest.options.trace_mode == 'src':
    expected_reports['monitor.ads.xcov'] = {'+': {2}}


def tryseq(label, scoargs):
    thistest.log('== {} =='.format(label))
    xcov(coverage_base_args + scoargs)
    check_xcov_reports('.', expected_reports)


if thistest.options.trace_mode == 'src':
    option = 'sid'
    ext = 'sid'
else:
    option = 'scos'
    ext = 'ali'

sco_files = ['obj/trymon.{}'.format(ext),
             'obj/monitor.{}'.format(ext)]
tryseq('one_response_file',
       scoargs=['--{}'.format(option), '@{}'.format(list_to_tmp(sco_files))])
tryseq('multiple_files',
       scoargs=['--{}={}'.format(option, ali) for ali in sco_files])
thistest.result()
