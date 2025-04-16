"""
Check that --warnings-as-errors works as expected.
"""

import re

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


tmp = Wdir("tmp_")


def check(slug, project, warning, extra_args):
    """
    Check that "gnatcov instrument" on the given project yields the expected
    warning, and that it exits with an error when --warnings-as-errors is
    passed.
    """

    # Run "gnatcov instrument" once with default arguments, just to check that
    # gnatcov succeeds and emits a warning.
    thistest.log(f"== {slug}: sanity check ==")
    log = f"{slug}-sc.txt"
    process = xcov_instrument(
        gprsw=GPRswitches(root_project=project),
        extra_args=extra_args,
        covlevel="stmt",
        register_failure=False,
        out=log,
    )
    thistest.fail_if(process.status != 0, "'gnatcov instrument' exit in error")
    thistest.fail_if_no_match(
        "'gnatcov instrument' output", warning, contents_of(log)
    )

    # Then pass --warnings-as-errors to check the exit code
    thistest.log(f"== {slug}: warnings-as-errors ==")
    log = f"{slug}-err.txt"
    process = xcov_instrument(
        gprsw=GPRswitches(root_project=project),
        covlevel="stmt",
        extra_args=["--warnings-as-errors"] + extra_args,
        register_failure=False,
        out=log,
    )
    thistest.fail_if(
        process.status == 0, "'gnatcov instrument' exit without error"
    )
    thistest.fail_if_no_match(
        "'gnatcov instrument' output", warning, contents_of(log)
    )


# Check the handling of warnings emitted before command line options (including
# --warnings-as-errors) are fully loaded: this happens for warnings emitted by
# LibGPR2.
#
# To trigger them, just try to load a project with both --target and --config
# arguments.  Note that we expect the warning twice: once for the wrapper
# gnatcov program, and one for the gnatcov64 program (both need to load the
# project).
project = gprfor(
    prjid="regular",
    mains=["main.adb"],
    srcdirs=["../regular"],
)
check(
    "project-warning",
    project,
    "suite.cgpr:.*: error: --target: 'foo' is different from the target value"
    " in the configuration project '.*'"
    "\nsuite.cgpr:.*: error: --target: 'foo' is different from the target"
    " value in the configuration project '.*'",
    extra_args=["--target=foo"],
)

# Create a project with a missing source file, on which the Ada instrumenter
# will complain. This checks the handling of warnings emitted after command
# line options are fully loaded.
check(
    "instrumenter-warning",
    gprfor(
        prjid="missing_srcfile",
        mains=["main.adb"],
        srcdirs=["../missing-srcfile"],
    ),
    re.escape(
        "warning: While instrumenting main.adb..."
        "\nwarning: Cannot find required source file: pkg.ads"
    ),
    extra_args=[],
)

thistest.result()
