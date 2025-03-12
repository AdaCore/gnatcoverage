"""
Check that gnatcov does not crash when passing an invalid target value when
only the C language is enabled.
"""

from SUITE.context import thistest
from SUITE.cutils import contents_of, Wdir
from SUITE.tutils import xcov

tmp = Wdir("tmp_")

# Run gnatcov setup with an invalid target, while disabling the Ada language
setup_log = "setup.log"
p = xcov(
    [
        "setup",
        "--target=INVALID",
        "--restricted-to-languages=C",
        "--prefix",
        "dummy",
    ],
    out=setup_log,
    auto_config_args=False,
    auto_languages=False,
    auto_target_args=False,
    register_failure=False,
)

thistest.fail_if(
    p.status == 0,
    "'gnatcov setup' did not reject the invalid target",
)
thistest.fail_if_no_match(
    "Unexpectect 'gnatcov setup' output",
    regexp=(
        "kb: info: can't find a toolchain for the following configuration:"
        " language 'Ada', target 'INVALID', default runtime\n"
        "kb: info: can't find a toolchain for the following configuration:"
        " language 'C', target 'INVALID', default runtime\n"
        ".*gnatcov(.exe)?: Cannot get library support for this configuration"
    ),
    actual=contents_of(setup_log).strip(),
)

thistest.result()
