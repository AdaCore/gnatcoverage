"""
Check that "gnatcov instrument" mentions the source file it is currently
instrumented when it is supposed to: once per source file that triggers an
error/warning, and once for every file in verbose mode.
"""

from e3.fs import mkdir

from SCOV.instr import xcov_instrument
from SUITE.cutils import contents_of, lines_of, Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import thistest, gprfor


Wdir("tmp_")

# Create the object directory to avoid spurious notices
gprsw = GPRswitches(root_project=gprfor(mains=["main.adb"], srcdirs=[".."]))
mkdir("obj")


def do_instr(label, args):
    thistest.log(f"== {label} ==")
    output_file = f"{label}.txt"
    p = xcov_instrument(
        gprsw=gprsw,
        covlevel="stmt",
        extra_args=args,
        out=output_file,
        register_failure=False,
    )
    thistest.fail_if(
        p.status != 0, f'{label}: "gnatcov instrument" unexpected failure'
    )
    return output_file


# Check that we print the source file that is currently instrumented when
# warnings/errors occur at most once, and never for other files.
log = do_instr("basic", [])
thistest.fail_if_not_equal(
    '"gnatcov instrument" output',
    contents_of(log).strip(),
    "\n".join(
        [
            "warning: While instrumenting main.adb...",
            "warning: Cannot find required source file: missing_a.ads",
            "warning: Cannot find required source file: missing_b.ads",
        ]
    ),
)

# In verbose mode: always print it once.
log = do_instr("verbose", ["-v"])
actual_lines = [
    line
    for line in lines_of(log)
    if (
        line.startswith("[GNATCOV.INSTRUMENT_SOURCES] Instrumenting ")
        or line.startswith("warning: ")
    )
]
thistest.fail_if_not_equal(
    '"gnatcov instrument" output',
    "\n".join(
        [
            "[GNATCOV.INSTRUMENT_SOURCES] Instrumenting main.adb",
            "warning: Cannot find required source file: missing_a.ads",
            "warning: Cannot find required source file: missing_b.ads",
            "[GNATCOV.INSTRUMENT_SOURCES] Instrumenting pkg.ads",
        ]
    ),
    "\n".join(actual_lines),
)

thistest.result()
