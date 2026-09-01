"""
Check the contents of XML reports to verify that coverage origins are correctly
reported.
"""

import dataclasses
import os
from typing import Any

from e3.fs import cp
import lxml.etree as etree

from SCOV.minicheck import build_and_run, xcov
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest

wd = Wdir("tmp_")

gpr = gprfor(["main_0.adb", "main_1.adb", "main_2.adb"], srcdirs="..")

os.mkdir("traces")


def delete_traces() -> None:
    for trace in os.scandir("traces"):
        os.remove(trace)


def check_xml(expected: str) -> None:
    # Create a copy of the XML report, so that all reports produced during the
    # test execution are available once the testcase has completed.
    label = expected.split(".")[0]
    copy_filename = f"process_{label}.adb.xml"
    cp(os.path.join("obj", "process.adb.xml"), copy_filename)

    # Now read the XML report and extract a summary from it focused on coverage
    # origins, so that we do not have to baseline the entire XML report.

    @dataclasses.dataclass
    class SCO:
        src: str
        kind: str
        origins: list[str]

    @dataclasses.dataclass
    class SrcMapping:
        src: str
        scos: list[SCO]

        @property
        def has_origins(self) -> bool:
            return any(s.origins for s in self.scos)

    def get_src(item: Any) -> str:
        """Return the source excerpt in an XML element."""
        src = item[0]
        assert src.tag == "src"
        line = src[0]
        assert line.tag == "line"
        return line.attrib["src"]

    src_mappings: list[SrcMapping] = []
    with open(copy_filename) as f:
        tree = etree.parse(f)
    for root_child in tree.getroot():
        if root_child.tag != "src_mapping":
            continue

        sm = SrcMapping(get_src(root_child), [])
        src_mappings.append(sm)

        def traverse(sm: SrcMapping, item: Any) -> None:
            """Look for SCOs in the given XML element."""
            for child in item:
                if child.tag not in {
                    "call",
                    "condition",
                    "contract_expression",
                    "decision",
                    "function",
                    "guarded_expr",
                    "statement",
                }:
                    continue

                sco = SCO(get_src(child), child.tag, [])
                sm.scos.append(sco)

                for grandchild in child:
                    if grandchild.tag == "origins":
                        for filepath in grandchild:
                            assert filepath.tag == "filepath"
                            sco.origins.append(filepath.text)
                traverse(sm, child)

        traverse(sm, root_child)

    # Write the extracted summary to a simple text file to compare against the
    # baseline.
    summary = f"summary_{label}.txt"
    with open(summary, "w") as f:
        for sm in src_mappings:
            if sm.has_origins:
                print(f"<src_mapping> for: {sm.src.strip()}", file=f)
                for sco in sm.scos:
                    print(f"  * SCO {sco.kind} for: {sco.src.strip()}", file=f)
                    for origin in sco.origins:
                        print(f"    origin: {origin}", file=f)

    thistest.fail_if_diff(
        os.path.join("..", expected), summary, ignore_white_chars=False
    )


def instr_and_run_zero_one() -> list[str]:
    # Instrument and run the two main executables.  Rename the resulting trace
    # files "zero.srctrace" and "one.srctrace" and place them in "traces/".
    xcov_args = build_and_run(
        gprsw=GPRswitches(root_project=gpr, units=["process"]),
        covlevel="stmt+mcdc+atcc+fun_call+gexpr",
        mains=["main_0", "main_1", "main_2"],
        extra_coverage_args=["-axml", "--origins"],
        extra_instr_args=["--dump-filename-simple", "--instrument-block"],
    )
    traces = sorted(xcov_args[-3:])
    os.rename(traces[0], "traces/zero.srctrace")
    os.rename(traces[1], "traces/one.srctrace")
    os.rename(traces[2], "traces/two.srctrace")
    return xcov_args[:-3]


def test_with_checkpoint() -> None:
    thistest.log("== test_with_checkpoint ==")
    xcov_args = instr_and_run_zero_one()

    # Create a checkpoint "c.ckpt" using the first two traces
    thistest.log("* create c.ckpt")
    xcov(
        xcov_args
        + [
            "traces/zero.srctrace",
            "traces/one.srctrace",
            "--save-checkpoint=c.ckpt",
        ]
    )

    # Commpute the coverage using "two.srctrace" and checkpoint "c.ckpt"
    thistest.log("* consolidate with c.ckpt")
    xcov(xcov_args + ["traces/two.srctrace", "--checkpoint=c.ckpt"])

    # Check the XML report against the expected result. We only expected to
    # find "two.srctrace" or "c.ckpt" as origins.
    check_xml("with_checkpoint.expected")


def test_only_traces() -> None:
    thistest.log("== test_only_traces ==")
    xcov_args = instr_and_run_zero_one()

    # Commpute the coverage using the three trace files
    xcov(xcov_args + ["traces/"])

    # Check the XML report against the expected result. We only expected to
    # find "two.srctrace" or "c.ckpt" as origins.
    check_xml("only_traces.expected")


# Run the tests

test_only_traces()
delete_traces()
test_with_checkpoint()
thistest.result()
