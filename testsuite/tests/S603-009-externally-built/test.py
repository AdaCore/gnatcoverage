"""
Check that GNATcoverage does not process externally built projects, unless
--externally-built-projects is passed.
"""

import os.path
import re
import shutil

from SCOV.instr import xcov_instrument
from SCOV.minicheck import build_and_run, check_xcov_reports
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of, indent
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprbuild, gprinstall, xcov


mylib_gpr = os.path.abspath(os.path.join('mylib', 'mylib.gpr'))
mylib_obj_dir = os.path.abspath(os.path.join('mylib', 'obj'))
main_dir = os.path.abspath('main')
main_gpr = os.path.join(main_dir, 'main.gpr')
main_obj_dir = os.path.join(main_dir, 'obj')

# Create the installation directory and add it to gprbuild's project lookup
# path.
tmp = Wdir('tmp_')
install_dir = os.path.abspath('install')
gpr_install_dir = os.path.join(install_dir, 'share', 'gpr')
installed_mylib_gpr = os.path.join(gpr_install_dir, 'mylib.gpr')
os.mkdir(install_dir)

old_path = os.environ.get('GPR_PROJECT_PATH', '')
if old_path:
    new_path = '{}{}{}'.format(gpr_install_dir, os.path.pathsep, old_path)
else:
    new_path = gpr_install_dir
os.environ['GPR_PROJECT_PATH'] = new_path

# Build and install the library project
if thistest.options.trace_mode == 'src':
    xcov_instrument(
        gprsw=GPRswitches(root_project=mylib_gpr),
        covlevel='stmt',
        gpr_obj_dir=mylib_obj_dir
    )
gprbuild(mylib_gpr)

if thistest.options.trace_mode == 'src':
    gprinstall(
        mylib_gpr,
        [
            f'--prefix={install_dir}',
            '--src-subdirs=gnatcov-instr',
            '--implicit-with=gnatcov_rts'
        ]
    )
else:
    gprinstall(mylib_gpr, f'--prefix={install_dir}')

# Build the main project using this and run it to produce a trace file
xcov_args = build_and_run(
    gprsw=GPRswitches(root_project=main_gpr, externally_built_projects=True),
    covlevel='stmt',
    mains=['main'],
    extra_coverage_args=['-axcov'],
    gpr_obj_dir=main_obj_dir,
    gpr_exe_dir=main_obj_dir,
    gprsw_for_coverage=False
)
trace_file = xcov_args[-1]

# Check that the instrumenter hasn't tried to instrument the installed project
thistest.fail_if(
    os.path.exists(os.path.join(install_dir,
                                'share', 'gpr', 'mylib-gnatcov-instr')),
    '"gnatcov instrument" instrumented the installed project'
)

output_dir = 'xcov-report'
gnatcov_log = 'gnatcov.log'


def check_coverage(project, externally_built, expected_cov,
                   register_failure=True):
    if os.path.isdir(output_dir):
        shutil.rmtree(output_dir)
    os.mkdir(output_dir)

    args = ['coverage', '-cstmt', '-axcov', '-P', project,
            '--output-dir', output_dir, trace_file]
    if externally_built:
        args.append('--externally-built-projects')
    p = xcov(args, out=gnatcov_log, register_failure=register_failure)

    if not register_failure:
        return p

    check_xcov_reports(os.path.join(output_dir, '*.xcov'), expected_cov)


# First, make sure that "gnatcov coverage" on an externally built project
# returns an error.
p = check_coverage(
    project=installed_mylib_gpr, externally_built=False, expected_cov=None,
    register_failure=False)
thistest.fail_if(
    p.status == 0,
    'gnatcov was supposed to complain when provided an externally built'
    ' project')
coverage_log = contents_of(gnatcov_log).strip()
expected_log = re.compile(
    # Regexp to accommodate output differences between the various
    # supported platforms.
    '[^\n]*gnatcov[^\n]*: {}'.format(re.escape(
        'Root project is marked as externally built, while externally built'
        ' projects are ignored by default. Consider using'
        ' --externally-built-projects.')))
thistest.fail_if(not expected_log.match(coverage_log),
                 'Unexpected output for "gnatcov coverage". Expected:\n'
                 '{}\n'
                 'but got:\n'
                 '{}'.format(indent(expected_log.pattern),
                             indent(coverage_log)))

# It should not complain with --externally-built-projects
p = check_coverage(
    project='mylib.gpr', externally_built=True,
    expected_cov={os.path.join(output_dir, 'mylib.adb.xcov'): {'+': {5, 6},
                                                               '-': {8}}})

# Make sure coverage computation gives the expected result with and without
# --externally-built-projects
check_coverage(
    project=main_gpr, externally_built=False,
    expected_cov={os.path.join(output_dir, 'main.adb.xcov'): {'+': {4, 6}}})
check_coverage(
    project=main_gpr, externally_built=True,
    expected_cov={os.path.join(output_dir, 'main.adb.xcov'): {'+': {4, 6}},
                  os.path.join(output_dir, 'mylib.adb.xcov'): {'+': {5, 6},
                                                               '-': {8}}})

thistest.result()
