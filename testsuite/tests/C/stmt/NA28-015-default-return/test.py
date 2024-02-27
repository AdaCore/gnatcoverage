from SCOV.minicheck import build_and_run
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import xcov, thistest
from SUITE.tutils import gprfor, srctracename_for, tracename_for

import re

Wdir("tmp_")

# Point here is to exercise consolidation over distinct executions of the same
# program with different command line arguments.  The SCOV circuitry isn't
# really suited for this so we're doing a rough approximation manually.

# This also exercises a specific sequence of C constructs, with an explicit
# return in the default alternative of a switch case.


def run_trace_with_args(prefix, exec_args):
    build_and_run(
        gprsw=GPRswitches(
            root_project=gprfor(srcdirs=[".."], mains=["calc.c"])
        ),
        covlevel="stmt",
        mains=["calc"],
        extra_coverage_args=[],
        extra_run_args=["-o", f"{prefix}.trace"],
        extra_instr_args=[
            f"--dump-filename-prefix={prefix}",
            "--dump-filename-simple",
        ],
        register_failure=False,
        exec_args=exec_args,
    )


def trycov(cases, xnocov):
    """
    Consolidate the set of traces whose basenames are provided in CASES.  Check
    that we have '-' notes on lines designated by exprs in XNOCOV, and only
    there.
    """

    fun_tracename = (
        srctracename_for
        if thistest.options.trace_mode == "src"
        else tracename_for
    )

    xcov(
        "coverage --level=stmt --annotate=xcov"
        " -P gen.gpr %s" % " ".join(fun_tracename(c) for c in cases)
    )

    def check_mark_on(line):
        m = re.search(string=line, pattern=r"^ *\d+ (?P<note>.):")
        if not m:
            return

        note = m.group("note")

        m = re.search(string=line, pattern="// # (?P<mark>.*)")
        mark = m.group("mark") if m else None

        thistest.fail_if(
            mark in xnocov and note != "-",
            "missing expected '-' note on line:\n%s" % line,
        )

        thistest.fail_if(
            note == "-" and mark not in xnocov,
            "unexpected '-' note on line:\n  %s" % line,
        )

    print("checking cases=%s" % str(cases))
    with open("obj/process.c.xcov", "r") as f:
        for line in f:
            check_mark_on(line=line.strip())


run_trace_with_args("plus", ["2", "3", "+"])
run_trace_with_args("mult", ["4", "5", "*"])
run_trace_with_args("unsupp", ["4", "5", "#"])

trycov(cases=["mult", "plus"], xnocov=["unsupp"])
trycov(cases=["plus"], xnocov=["mult", "unsupp"])
trycov(cases=["mult"], xnocov=["plus", "unsupp"])
trycov(cases=["mult", "plus", "unsupp"], xnocov=[])

thistest.result()
