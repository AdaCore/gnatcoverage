"""
Expose a "check" conveniency function to all testcases.
"""

import os

from SUITE.gprutils import GPRswitches
from SUITE.cutils import to_list, Wdir
from SCOV.tc import TestCase
from SCOV.tctl import CovControl

# Helpers for the common "check" function below

# The set of source file names with coverage obligations attached to
# each project, designated by project short name.
_xreports = {
    'boolops':  ['boolops.ads', 'boolops.adb',
                 'boolops-andthen.adb', 'boolops-orelse.adb'],
    'intops':   ['intops.ads', 'intops.adb',
                 'intops-add.adb', 'intops-sub.adb'],
    'counters': ['counters.ads', 'counters.adb']
    }


def _gpr_for(prj):
    """
    Assuming we're in the tmpdir created by the SCOV TestCase
    instance, return the relative file name for the project file
    corresponding to the `prj` project shortname.
    """

    # <tcgroup_dir>
    #     /intops
    #     /boolops
    #     /counters
    #     /<tcdir>/<tmpdir_for_check>/<tmpdir_by_TestCase>

    return '../../../%s/%s.gpr' % (prj, prj)


def check(root_project, recurse,
          projects=None, units=None,
          xreports=None):
    """
    Check that running our test with

       -P`root_project`
       [--projects=... for `projects`]
       [--units=... for `units`]
       [--no-subprojects] (if `recurse` is False)

    we obtain reports for the units attached to the projects listed
    in `xreports`.

    If not None, `projects` and `xreports` are expected to be lists of
    shortcut names like 'boolops', 'intops' or 'counters'. This function
    takes care of converting them to relative project file names actually
    expected on the command line or in real project file dependencies.

    `root_project` may be either a .gpr filename, in which case it is used
    as-is, or a project short name.

    `recurse` None means "arrange not to pass any option influencing
    recursiveness".
    """

    projects = to_list(projects)
    units = to_list(units)

    # root_project, projects, and units arguments we will provide to the
    # GPRswitches class:
    gprsw_root_project = (
        root_project if root_project.endswith('.gpr')
        else _gpr_for(root_project))

    gprsw_projects = [_gpr_for(prj) for prj in projects]

    gprsw_units = units

    # Arrange to execute each check in its own tmp dir and
    # passing a unique --subdirs prevent mixups across test variants
    # within the shared projects.

    # Start with 'wd_foo' from .../.../foo.gpr or a project short
    # name intended for -P.
    tmpdir = 'wd_' + os.path.basename(root_project).split('.')[0]

    # Append the first letter of each project name will pass through
    # --project, if any:
    if projects:
        tmpdir += '-' + ''.join(prj[0] for prj in projects)

    # Append indication on recursion request:
    if recurse:
        tmpdir += '-rt'
    elif recurse is None:
        tmpdir += '-rn'
    else:
        tmpdir += '-rf'

    # For the --subdirs argument, relative to each subproject's object dir,
    # prepend our testcase local directory name:
    gprsw_subdirs = os.path.basename(os.getcwd()) + '_' + tmpdir

    # If a list of expected reports is provided, convert into list of
    # corresponding sources, which the CovControl class expects:

    if xreports is not None:
        ctl_xreports = []
        for xr in xreports:
            ctl_xreports.extend(
                _xreports[xr] if xr in _xreports else [xr])
    else:
        ctl_xreports = None

    # Getting the default behavior wrt recursiveness consists
    # in requesting not to pass --no-subprojects.
    gprsw_no_subprojects = False if recurse is None else not recurse

    wd = Wdir(clean=True)
    wd.to_subdir(tmpdir)

    TestCase(category=None).run(
        covcontrol=CovControl(

            # The programs we build and exercise alway depend on
            # the three subprojects:
            deps=[_gpr_for('boolops'),
                  _gpr_for('intops'),
                  _gpr_for('counters')],

            # What we analyse and check depends on our arguments:
            gprsw=GPRswitches(
                root_project=gprsw_root_project,
                projects=gprsw_projects,
                units=gprsw_units,
                no_subprojects=gprsw_no_subprojects,
                subdirs=gprsw_subdirs),

            xreports=ctl_xreports,

            # The test driver and the likes are never of interest
            units_in=[]))

    wd.to_homedir()
