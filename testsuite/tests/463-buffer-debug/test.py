"""
Check that --debug-dump generates the expected JSON data.
"""

from e3.testsuite.driver.diff import (
    LineByLine,
    CanonicalizeLineEndings,
)
import json

from SCOV.minicheck import xcov_instrument
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of, exists, AbsoluteToBasenameRefiner
from SUITE.gprutils import GPRswitches
from SUITE.tutils import cmdrun, gprbuild, gprfor

tmp = Wdir("tmp_")

DEBUG_FILE = "dump.json"

PRJ_FILE = gprfor(
    prjid="proj", mains=["main.adb", "c-main.c"], srcdirs=["../src"]
)
HARNESS_FILE = gprfor(
    prjid="harness",
    mains=["main-harness.c"],
    srcdirs=["../harness-src", "."],
    deps=["proj"],
)


def generate_buffer_list(bufs: list[str]):
    TEMPLATE = """
    #include "gnatcov_rts_c-buffers.h"

    %BUFFER_SYMBOL_DECLS%

    // generated_buffer_list.c
    struct gnatcov_rts_coverage_buffers_group *BUFFERS[] = {
    %BUFFER_SYMBOLS%
    NULL
    };
    """

    with open("generated.c", "w") as f:
        symbols = "".join(f"&{buf},\n" for buf in bufs)
        decls = "".join(
            f"extern struct gnatcov_rts_coverage_buffers_group {buf};\n"
            for buf in bufs
        )
        _ = f.write(
            TEMPLATE.replace("%BUFFER_SYMBOLS%", symbols).replace(
                "%BUFFER_SYMBOL_DECLS%", decls
            )
        )


# Generate a dummy `generated.c` with an empty list
generate_buffer_list([])

# Instrument the project and dump the JSON debug info
p = xcov_instrument(
    gprsw=GPRswitches(root_project=HARNESS_FILE),
    covlevel="stmt",
    extra_args=["--projects=proj", "--dump-debug", DEBUG_FILE],
)

# Fail if the JSON file wasn't generated
thistest.fail_if(
    not exists(DEBUG_FILE), f"expected file {DEBUG_FILE} not found"
)

# Parse the debug file and extract the unit names and their buffer symbol.
js: dict[str, dict[str, dict[str, str]]] = json.loads(contents_of(DEBUG_FILE))
buffer_symbol_list = [
    unit_info["buffers"] for unit_info in js["buffer_symbols"].values()
]

# Write a `generated.c` file where we declare an `extern` reference to the
# buffer groups and then print them.
generate_buffer_list(buffer_symbol_list)

# Build the project with the new `generated.c`
p_build = gprbuild(project=HARNESS_FILE)

# Run the program and check its output
p_run = cmdrun(["./main-harness"], for_pgm=True, out="actual.out")


# Compare the list of units with the ones expected from the baseline
thistest.fail_if_diff(
    "../baseline.out",
    "actual.out",
    "List of units differs from baseline",
    output_refiners=[
        # For C files, get rid of the absolute path
        LineByLine(AbsoluteToBasenameRefiner()),
        # Ignore platform specificities
        CanonicalizeLineEndings(),
    ],
)

thistest.result()
