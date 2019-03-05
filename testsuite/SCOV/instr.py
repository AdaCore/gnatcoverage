"""
Helpers to test instrumentation-based source coverage.
"""

import collections
import os.path

from SUITE.tutils import XCOV, exepath_to, xcov


class InstrumentedProject(object):
    def __init__(self, gpr_file, obj_dir, covlevel):
        """
        :param str gpr_file: Project file for the instrumented sources.
        :param str obj_dir: Directory that contains executables for the
            instrumented mains.
        :param str covlevel: Coverage level for the instrumentation (--level
            argument).
        """
        self.gpr_file = gpr_file
        self.obj_dir = obj_dir
        self.covlevel = covlevel

    def exepath_to(self, program_name):
        """
        Return the absolute path to the executable corresponding to the
        `program_name` unit in this instrumented project.

        :type program_name: str
        :rtype: str
        """
        return exepath_to(os.path.join(self.obj_dir, program_name))


def xcov_instrument(root_project, obj_dir, covlevel, checkpoint, projects=[],
                    units=[], recursive=False, auto_dump_buffers=True,
                    out=None, err=None, register_failure=True):
    """
    Run "gnatcov instrument" on a project.

    :param str root_project: Project to pass to gnatcov (-P).
    :param str obj_dir: Object directory for that project.
    :param str covlevel: Coverage level for the instrumentation (--level
        argument).
    :param str checkpoint: Name of the checkpoint file to create.
    :param list[str] projects: Optional list of projects for units of
        interests (--project argument).
    :param list[str] units: Optional list of units of interest (--units
        argument).
    :param bool recursive: Whether to process projects and their closures of
        dependencies (not done by default, --recursive option).
    :param bool auto_dump_buffers: Whether to instrument main sources to add a
        dump of coverage buffers at the end (done it by default,
        --auto-dump-buffers option).

    See SUITE.tutils.xcov for the other supported options.

    :rtype: InstrumentedProject
    """
    args = ['instrument',
            '--level={}'.format(covlevel),
            '-P{}'.format(root_project)]

    for p in projects:
        args.append('--project={}'.format(p))
    for u in units:
        args.append('--units={}'.format(u))
    if recursive:
        args.append('--recursive')
    if auto_dump_buffers:
        args.append('--auto-dump-buffers')

    args.append(checkpoint)

    xcov(args, out=out, err=err, register_failure=register_failure)
    gpr_prefix = os.path.join(obj_dir, 'gnatcov-instr')
    return InstrumentedProject(
        os.path.join(gpr_prefix, 'instrumented.gpr'),
        os.path.join(gpr_prefix, 'obj-instr'),
        covlevel
    )
