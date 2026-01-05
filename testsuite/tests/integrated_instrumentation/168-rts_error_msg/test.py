"""
Test that the error message emitted by the "gnatcov setup-integration" command
is helpful when failing to load the coverage runtime project.
"""

from SUITE.cutils import Wdir
from SUITE.tutils import contents_of, thistest, xcov

tmp = Wdir("tmp_")

# Try to setup for a simple main file
integration_log = "setup-integration.log"
p = xcov(
    [
        "setup-integration",
        "-cstmt+mcdc",
        "--output-dir=.",
        "--files=../main.c",
        "--runtime-project=no_such_gnatcov_rts",
    ],
    out=integration_log,
    register_failure=False,
)

thistest.fail_if(p.status == 0, "gnatcov exit status shouldn't be success")

# Check that the error message correctly reports an issue with the coverage
# runtime.
thistest.fail_if_no_match(
    what="gnatcov error message",
    regexp=(
        'error: project file "no_such_gnatcov_rts\\.gpr" not found\n'
        "The following directories have been searched:\n"
        "(\n|.)*\n"
        ".*gnatcov.*: Could not load the coverage runtime project"
        " no_such_gnatcov_rts$"
    ),
    actual=contents_of(integration_log).strip(),
)

thistest.result()
