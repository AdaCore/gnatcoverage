"""
Perform a bunch of xcov related operations to validate the basic toolset
functionalities.
"""

from SUITE.context import thistest
from SUITE.control import target_info
from SUITE.cutils import Wdir, match
from SUITE.tutils import (
    exepath_to,
    gprbuild,
    gprfor,
    maybe_valgrind,
    tracename_for,
    xcov,
    xrun,
)


Wdir("tmp_")

# Context information, basic command line interface checks
print("maybe_valgrind prepends ... " + str(maybe_valgrind([])))
xcov(["--version"])
xcov(["--help"])

# Core functions checks over the Engines example

# We will first work on a set of operations around the "engines" simple
# example, with different executables to exercise various combinations of
# value/thresold relationships on pressure and temperature.
#
# Each variant sets the engine P/T parameters to the P/T stability threshold
# plus or minus one and checks that the Stable function returns the expected
# value. Doing so, it exercises a subset of the possible path combinations
# through the function return boolean expression.
#
# We name each variant with a couple of letters, one per Pressure/Temperature
# parameter, each either P or M to denote whether the parameter is set to the
# threshold Plus or Minus one.
variants = ["mm", "mp", "pm", "pp"]


# The sources are named accordingly (e.g. test_engines_mm.adb), and we we play
# with a number of related outputs (test_engines_mm executable, .trace file
# out of qemu, etc.).  We Define a class to help handle such variants:


class Vtest:
    def __init__(self, basename, variant):
        self.basename = basename
        self.variant = variant

    def filename(self, suffix=""):
        """
        Return the name to use for a file related to this variant, suffixed
        with SUFFIX.
        """
        return self.basename + "_" + self.variant + suffix

    def tag(self):
        """Return a trace tag to associate with this test variant."""
        return "%s, %s variant" % (self.basename, self.variant)


# Build all the executable variants from a single multi-mains project file
gprbuild(
    gprfor(
        [Vtest("test_engines", v).filename(".adb") for v in variants],
        srcdirs="../src",
    )
)

# First, a number of tests over individual variants:
for variant in variants:
    vt = Vtest("test_engines", variant)

    # Xrun & stick a dedicated tag into the trace output
    xrun([exepath_to(vt.filename()), "--tag=" + vt.tag()])

    # Compute branch coverage for this variant & check that there's only
    # partial coverage of the return expression
    xcov(
        [
            "coverage",
            "--level=branch",
            "--annotate=xcov",
            vt.filename(tracename_for("")),
        ]
    )
    thistest.fail_if(
        not match(r" \!:.*return.*Stable_P", "engines.adb.xcov"),
        "single input, expected partial coverage over return expr",
    )

    # Verify that we can dump the trace and find the tag in the dump output
    xcov(
        ["dump-trace", tracename_for(vt.filename())], vt.filename(".tracedump")
    )
    thistest.fail_if(
        not match(
            "Tag.*(User_Tag).*\nLen.*\nData.*%s" % vt.tag(),
            vt.filename(".tracedump"),
        ),
        "single input, expected user tag from --dump-trace",
    )

# Now, check that we can combine the trace results from all the variants
# and obtain full branch coverage of the return boolean expression. Build
# a consolidation routine list for this purpose.
with open("list.stable", "w") as rl:
    rl.write(target_info().to_platform_specific_symbol("engines__stable\n"))

xcov(
    [
        "coverage",
        "--level=branch",
        "--annotate=xcov",
        "--routines=@list.stable",
    ]
    + [tracename_for(Vtest("test_engines", v).filename()) for v in variants]
)
thistest.fail_if(
    not match(r" \+:.*return.*Stable_P", "engines.adb.xcov"),
    "combined inputs, expected full branch coverage over return expr",
)

# Check that we can produce html without crashing as well ...
xcov(
    [
        "coverage",
        "--level=branch",
        "--annotate=html+",
        "--routines=@list.stable",
    ]
    + [Vtest("test_engines", v).filename(tracename_for("")) for v in variants]
)

thistest.result()
