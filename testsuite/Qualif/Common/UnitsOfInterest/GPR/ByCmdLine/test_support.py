"""
Expose a "check" conveniency function to all testcases.
"""

import os

from e3.fs import mkdir, sync_tree

from SCOV.tc import TestCase
from SCOV.tctl import CovControl
from SUITE.control import env, gnatemu_board_name
from SUITE.cutils import to_list, Wdir
from SUITE.gprutils import GPRswitches

# Helpers for the common "check" function below

# The set of source file names with coverage obligations attached to
# each project, designated by project short name.
_xreports = {
    "boolops": [
        "boolops.ads",
        "boolops.adb",
        "boolops-andthen.ads",
        "boolops-andthen.adb",
        "boolops-orelse.ads",
        "boolops-orelse.adb",
    ],
    "intops": [
        "intops.ads",
        "intops.adb",
        "intops-add.ads",
        "intops-add.adb",
        "intops-sub.ads",
        "intops-sub.adb",
    ],
    "counters": ["counters.ads", "counters.adb"],
}
all_projects = list(_xreports)


def check(root_project, recurse, projects=None, units=None, xreports=None):
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

    # Create a label for this variant that is unique in this testcase. Start
    # including the root project.
    label = os.path.splitext(os.path.basename(root_project))[0]

    # Append the first letter of each project name will pass through
    # --project, if any:
    if projects:
        label += "-" + "".join(prj[0] for prj in projects)

    # Append indication on recursion request:
    if recurse:
        label += "-rt"
    elif recurse is None:
        label += "-rn"
    else:
        label += "-rf"

    # Arrange to execute each check in its own temporary directory and copying
    # shared projects in that directory prevent mixups across test variants.
    tmpdir = f"tmp_{label}"
    wd = Wdir(tmpdir)

    # Copy shared projects in the temporary directory and create their object
    # directory to avoid spurious warnings.
    for p in all_projects:
        sync_tree(os.path.join(wd.homedir, "..", p), p)
        mkdir(os.path.join(p, "obj"))

    # If a list of expected reports is provided, convert into list of
    # corresponding sources, which the CovControl class expects:
    if xreports is not None:
        ctl_xreports = []
        for xr in xreports:
            ctl_xreports.extend(_xreports[xr] if xr in _xreports else [xr])
    else:
        ctl_xreports = None

    # Getting the default behavior wrt recursiveness consists in requesting not
    # to pass --no-subprojects.
    no_subprojects = False if recurse is None else not recurse

    TestCase(category=None).run(
        covcontrol=CovControl(
            # The programs we build and exercise always depend on the three
            # subprojects (copied above in the parent directory relative to the
            # TestCase temporary directory).
            deps=[f"../{p}/{p}" for p in all_projects],
            # What we analyse and check depends on our arguments:
            gprsw=GPRswitches(
                root_project=root_project,
                projects=projects,
                units=units,
                no_subprojects=no_subprojects,
                xvars=[("BOARD", gnatemu_board_name(env.target.machine))],
            ),
            xreports=ctl_xreports,
            # The test driver and the likes are never of interest
            units_in=[],
        ),
    )

    wd.to_homedir()
