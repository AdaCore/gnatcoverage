"""
Helpers to test instrumentation-based source coverage.
"""

import collections
import os.path

from SUITE.tutils import XCOV, exepath_to, xcov


def xcov_instrument(root_project, covlevel, checkpoint, projects=[], units=[],
                    recursive=False, auto_dump_buffers=True, out=None,
                    err=None, register_failure=True):
    """
    Run "gnatcov instrument" on a project.

    :param str root_project: Project to pass to gnatcov (-P).
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
