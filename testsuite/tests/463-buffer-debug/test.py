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
from SUITE.tutils import gprbuild, gprfor, run_cov_program

tmp = Wdir("tmp_")

DEBUG_FILE = "dump.json"

PRJ_FILE = gprfor(
    prjid="proj", mains=["main.adb", "c-main.c"], srcdirs=["../src"]
)
HARNESS_FILE = gprfor(
    prjid="harness",
    mains=["harness.adb"],
    srcdirs=["../harness-src", "."],
    deps=["proj", "gnatcov_rts"],
)


def generate_buffer_list(bufs: list[str]):
    items = []
    decls = []
    for buf in bufs:
        new_id = f"G{len(items)}"
        items.append(f"{new_id}'Access")
        decls += [
            f"{new_id} : aliased constant GNATcov_RTS_Coverage_Buffers_Group;",
            f'pragma Import (C, {new_id}, "{buf}");',
        ]
    contents = f"""
        with GNATcov_RTS.Buffers.Lists; use GNATcov_RTS.Buffers.Lists;

        package Generated is
            {"\n".join(decls)}

            Buffers_Groups : constant Coverage_Buffers_Group_Array := (
              {", ".join(items)}
            );
        end Generated;
    """
    with open("generated.ads", "w") as f:
        f.write(contents)


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
p_run = run_cov_program("./harness", out="actual.out")


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
