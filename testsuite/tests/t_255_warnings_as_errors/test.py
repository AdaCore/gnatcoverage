"""
Check that --warnings-as-errors works as expected.
"""

import re
from typing import Callable

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprfor
from SUITE.gprutils import GPRswitches


def check(
    slug: str,
    create_project: Callable[[], str],
    warning: str,
    extra_args: list[str],
) -> None:
    """
    Check that "gnatcov instrument" on the given project yields the expected
    warning, and that it exits with an error when --warnings-as-errors is
    passed.
    """
    tmp = Wdir(f"tmp_sc_{slug}")

    # Run "gnatcov instrument" once with default arguments, just to check that
    # gnatcov succeeds and emits a warning.
    thistest.log(f"== {slug}: sanity check ==")
    log = f"{slug}-sc.txt"
    process = xcov_instrument(
        gprsw=GPRswitches(root_project=create_project()),
        extra_args=extra_args,
        covlevel="stmt",
        register_failure=False,
        out=log,
    )
    thistest.fail_if(process.status != 0, "'gnatcov instrument' exit in error")
    thistest.fail_if_no_match(
        "'gnatcov instrument' output", warning, contents_of(log)
    )
    tmp.to_homedir()

    # Then pass --warnings-as-errors to check the exit code
    tmp = Wdir(f"tmp_wae_{slug}")
    thistest.log(f"== {slug}: warnings-as-errors ==")
    log = f"{slug}-err.txt"
    process = xcov_instrument(
        gprsw=GPRswitches(root_project=create_project()),
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
    tmp.to_homedir()


# Check the handling of warnings emitted before command line options (including
# --warnings-as-errors) are fully loaded: this happens for warnings emitted by
# LibGPR2.
#
# To trigger them, just try to load a project with both --target and --config
# arguments.  Note that we expect the warning twice: once for the wrapper
# gnatcov program, and one for the gnatcov64 program (both need to load the
# project).
check(
    "project-warning",
    lambda: gprfor(
        prjid="regular",
        mains=["main.adb"],
        srcdirs=["../regular"],
    ),
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
    lambda: gprfor(
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


def create_instr_limit_prj() -> str:
    return gprfor(
        prjid="instr_limitation",
        mains=["main.adb"],
        srcdirs=["../instr-limitation"],
    )


# Check that instrumenter limitations do trigger the warnings-as-error
# mechanism...
check(
    "instrumenter-limitation",
    create_instr_limit_prj,
    r"\?\?\? main.adb:7:7: gnatcov limitation: cannot instrument an"
    " expression function which is a primitive of its return type, when"
    " this type is a tagged type. Consider turning it into a regular function"
    " body.",
    extra_args=[],
)

# .. Except when --suppress-limitations is passed to 'gnatcov instrument',
# in which case no message should be emitted at all.
thistest.log("== instrumenter-limitation: suppress-limitations ==")
tmp = Wdir("tmp_sl_instrumenter-limitation")
p = xcov_instrument(
    gprsw=GPRswitches(root_project=create_instr_limit_prj()),
    covlevel="stmt",
    extra_args=["--suppress-limitations"],
)
thistest.fail_if_not_equal(
    "'gnatov instrument' failed with --suppress-limitations",
    p.status,
    0,
)
tmp.to_homedir()

thistest.result()
