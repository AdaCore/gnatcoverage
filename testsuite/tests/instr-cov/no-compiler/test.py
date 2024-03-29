"""
Check that gnatcov properly reports a missing C/C++ compiler in the
environment. It used to emit a cryptic error message: invalid path "".
"""

from e3.os.process import DEVNULL, Run, STDOUT

from SCOV.instr import xcov_instrument
from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

# Create a configuration file so that only the Ada compiler is available to
# gnatcov.
config_file = "for_ada.cgpr"
Run(
    [
        "gprconfig",
        "--batch",
        f"--target={env.target.triplet}",
        "-o",
        config_file,
        f"--config=Ada,,{thistest.options.RTS or ''}",
    ],
    input=DEVNULL,
    output="gprconfig.out",
    error=STDOUT,
)

gpr = GPRswitches(root_project=gprfor(srcdirs=[".."], mains=["main.c"]))

# Now run the instrumenter on a C project with this configuration file and
# check the error message.
thistest.log("== Instrument all ==")
output = "instr_0.log"
p = xcov_instrument(
    gprsw=gpr,
    covlevel="stmt",
    extra_args=[f"--config={config_file}"],
    out=output,
    auto_config_args=False,
    auto_target_args=False,
    register_failure=False,
)
thistest.fail_if_not_equal("'gnatcov instrument' exit code", 1, p.status)
thistest.fail_if_no_match(
    "'gnatcov instrument' output",
    ".*gnatcov.*: could not find a compiler for C",
    contents_of(output).strip(),
)


# Likewise, but this time with only Ada units that can be of interest: the C
# source will be instrumented as a main only.
thistest.log("== Instrument Ada only ==")
output = "instr_1.log"
p = xcov_instrument(
    gprsw=gpr,
    covlevel="stmt",
    extra_args=[f"--config={config_file}", "--restricted-to-languages=Ada"],
    out=output,
    auto_config_args=False,
    auto_target_args=False,
    register_failure=False,
)
thistest.fail_if_not_equal("'gnatcov instrument' exit code", 1, p.status)
thistest.fail_if_no_match(
    "'gnatcov instrument' output",
    ".*gnatcov.*: could not find a compiler for C",
    contents_of(output).strip(),
)

thistest.result()
