"""
Check that gnatcov rejects invalid configurations for overlapping SCO
statements.
"""

from dataclasses import dataclass

from e3.fs import mkdir

from SCOV.instr import xcov_instrument
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import thistest, gprfor


@dataclass
class W:
    """Helper to represent expected warnings."""

    col: str
    sco_num: str
    col_range: str

    def __str__(self):
        return (
            f"!!! foo.c:5:{self.col}:"
            f" unexpected SCO overlapping with SCO {self.sco_num}:"
            f" STATEMENT at foo.c:5:{self.col_range},"
            " discarding overlapping SCO"
        )


def run_variant(label, col_ranges, expected_warnings):
    """
    Generate C code that will generate SCOS for the column number ranges in
    ``col_ranges``, instrument it and check that gnatcov emits the given
    expected warnings.
    """
    thistest.log(f"== {label} ==")
    wd = Wdir("tmp_" + label)

    # Generate C code and instrument it

    with open("foo.c", "w") as f:
        f.write("void\n")
        f.write("foo (void (*f) (const char *str))\n")
        f.write("{\n")
        for start, end in col_ranges:
            # Add an empty line after line directive to prevent the
            # postprocessing step in gnatcov from merging the lines together
            # (thus undoing the SCO nesting).
            f.write("#line 4\n\n")

            # Make sure the range is wide enough for us to insert a statement
            # (a call to the "f" function with a string literal argument).
            range_length = end - start + 1
            arg_length = range_length - 6
            assert arg_length >= 0

            f.write(" " * (start - 1))
            f.write('f("')
            f.write("A" * arg_length)
            f.write('");')
            f.write("\n")
        f.write("}\n")

    log = "instr.log"
    mkdir("obj")
    xcov_instrument(
        gprsw=GPRswitches(
            root_project=gprfor(mains=["test.c"], srcdirs=["..", "."]),
            units=["foo.c"],
        ),
        covlevel="stmt",
        out=log,
        tolerate_messages="unexpected SCO overlap",
    )

    # Check that the instrumenter emits the expected warnings. For convenience
    # and platform independence, harmonize line terminators in the output.
    thistest.fail_if_not_equal(
        '"gnatcov instrument" warnings',
        "\n".join(str(w) for w in expected_warnings),
        "\n".join(line.rstrip() for line in contents_of(log).splitlines())
    )

    wd.to_homedir()


run_variant("no-overlap", [(1, 10), (11, 20)], [])
run_variant("simple-nesting", [(1, 20), (2, 19)], [])
run_variant("contiguous-nesting", [(1, 20), (2, 10), (11, 19)], [])
run_variant("reverse-contiguous-nesting", [(11, 19), (2, 10), (1, 20)], [])

run_variant("nok-nesting-left", [(1, 20), (1, 19)], [W("1", "#1", "1-20")])
run_variant("nok-nesting-right", [(1, 20), (2, 20)], [W("2", "#1", "1-20")])
run_variant("overlap-left", [(1, 10), (5, 15)], [W("5", "#1", "1-10")])
run_variant("overlap-right", [(5, 15), (1, 10)], [W("1", "#1", "5-15")])
run_variant(
    "nested-overlap", [(1, 20), (10, 19), (2, 10)], [W("2", "#2", "10-19")]
)

thistest.result()
