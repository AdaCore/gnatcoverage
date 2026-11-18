"""
Check the contents of XML reports to verify that coverage origins are correctly
reported. Check that collecting origins scales to large trace batches.
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


def check_xml(
    expected: str, expected_files: list[str], main_origins: dict[str, str]
) -> None:
    """
    Check that the XML report contains the expected coverage origin
    information.

    :param expected: The baseline file to compare against. It contains a
        summary of the coverage origin information as follows::

            <src_mapping> for: Dummy : Integer := 0;
              * SCO statement for: Dummy : Integer := 0;
                origin: c.ckpt
                origin: traces/two.srctrace

    :param expected_files: The list of origins (traces, or checkpoints) that
        we expect in the XML report.

    :param main_origins: A mapping indicating the coverage origin for each
        main source.
    """
    thistest.log(f"Checking XML report against {expected}")
    report = etree.parse("obj/index.xml")
    report.xinclude()
    schema = etree.XMLSchema(etree.parse("obj/gnatcov-xml-report.xsd"))
    schema.assertValid(report)

    # Retrieve the origins mapping - the mapping from the ID to the actual
    # filename, and make sure each entry has a unique filename, and a unique
    # ID.
    files = report.findall("./coverage_report/coverage_info/origin_files/file")
    ids = [int(entry.attrib["id"]) for entry in files]
    filenames = [entry.attrib["filename"] for entry in files]
    thistest.fail_if_not_equal(
        "Origin filenames", expected_files, sorted(filenames)
    )
    thistest.fail_if_not_equal(
        "Unique origin filenames", len(filenames), len(set(filenames))
    )
    thistest.fail_if_not_equal(
        "Consecutive origin IDs", list(range(1, len(files) + 1)), ids
    )
    origin_files = dict(zip(ids, filenames))
    thistest.log(f"Origin filename table: {origin_files}")

    sources = report.findall("./coverage_report/sources/source")
    thistest.fail_if_not_equal(
        "Reported source files",
        ["main_0.adb", "main_1.adb", "main_2.adb", "process.adb"],
        sorted(os.path.basename(source.attrib["file"]) for source in sources),
    )

    # Loop over the sources, and check coverage origin information consistency:
    #   * Check that the coverage origin information does not have duplicate
    #     entries
    #   * Check that we have the right coverage origin for each main source
    for source in sources:
        name = os.path.basename(source.attrib["file"])
        thistest.log(f"Checking origin references in {name}")
        source_origins: set[str] = set()

        # The origins field is a set of IDs referencing origins - traces or
        # checkpoints.
        for origins in source.findall(".//origins"):
            references = [int(value) for value in origins.text.split()]
            sco = origins.getparent()
            context = f"{name}, {sco.tag} #{sco.attrib['id']}"

            # Check uniqueness: the same origin shall not appear twice
            thistest.log(f"  {context}: origin IDs {references}")
            thistest.fail_if_not_equal(
                f"Unique origin references for {context}",
                sorted(set(references)),
                sorted(references),
            )
            resolved = [origin_files[ref] for ref in references]
            thistest.log(f"    Resolved filenames: {resolved}")
            source_origins.update(resolved)

        # If this is a main, check that we have the expected origin reference,
        # according to main_origins.
        thistest.log(f"  Collected origins: {sorted(source_origins)}")
        if name in main_origins:
            thistest.log(f"  Expected origin: {main_origins[name]}")
            thistest.fail_if_not_equal(
                f"Shared origin table references for {name}",
                {main_origins[name]},
                source_origins,
            )

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
                        sco.origins.extend(
                            origin_files[int(ref)]
                            for ref in grandchild.text.split()
                        )
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
                    for origin in sorted(sco.origins):
                        print(f"    origin: {origin}", file=f)

    thistest.fail_if_diff(
        os.path.join("..", expected), summary, ignore_white_chars=False
    )


def instr_and_run_zero_one() -> list[str]:
    """Build and run the three mains, placing their traces in traces/.

    Return coverage options with --origins enabled, but without trace inputs
    or an annotation format. Each caller selects these for its own report.
    """
    xcov_args = build_and_run(
        gprsw=GPRswitches(
            root_project=gpr,
            units=["process", "main_0", "main_1", "main_2"],
        ),
        covlevel="stmt+mcdc+atcc+fun_call+gexpr",
        mains=["main_0", "main_1", "main_2"],
        extra_coverage_args=["--origins"],
        extra_instr_args=["--dump-filename-simple", "--instrument-block"],
    )
    traces = sorted(xcov_args[-3:])
    os.rename(traces[0], "traces/zero.srctrace")
    os.rename(traces[1], "traces/one.srctrace")
    os.rename(traces[2], "traces/two.srctrace")
    return xcov_args[:-3]


def check_scaling(xcov_args: list[str]) -> None:
    """Catch quadratic work as the number of coverage origins grows.

    Copies of one trace under distinct filenames grow the covered obligations'
    origin sets without changing coverage. The batch is sized so the former
    code, which copied the growing sets for every input, hits the test timeout.

    xcov_args provides common coverage options, including --origins, without
    trace inputs or an annotation format.
    """

    args = xcov_args + ["-axml"]
    os.mkdir("scaling_traces")
    traces = [f"scaling_traces/{index:05}.srctrace" for index in range(20000)]
    for trace in traces:
        cp("traces/zero.srctrace", trace)
    with open("scaling_traces.txt", "w") as f:
        f.write("\n".join(traces) + "\n")
    xcov(args + ["--trace=@scaling_traces.txt"])
    origins = etree.parse("obj/process.adb.xml").findtext(".//origins", "")
    thistest.fail_if_not_equal(
        "Scaling origin count", len(traces), len(origins.split())
    )


def test_with_checkpoint() -> None:
    thistest.log("== test_with_checkpoint ==")
    xcov_args = instr_and_run_zero_one()

    # Create a checkpoint "c.ckpt" using the first two traces
    thistest.log("* create c.ckpt")
    xcov(
        xcov_args
        + [
            "-axml",
            "traces/zero.srctrace",
            "traces/one.srctrace",
            "--save-checkpoint=c.ckpt",
        ]
    )

    # Commpute the coverage using "two.srctrace" and checkpoint "c.ckpt"
    thistest.log("* consolidate with c.ckpt")
    xcov(xcov_args + ["-axml", "traces/two.srctrace", "--checkpoint=c.ckpt"])

    # Check the XML report against the expected result. We only expected to
    # find "two.srctrace" or "c.ckpt" as origins.
    check_xml(
        "with_checkpoint.expected",
        ["c.ckpt", "traces/two.srctrace"],
        {
            "main_0.adb": "c.ckpt",
            "main_1.adb": "c.ckpt",
            "main_2.adb": "traces/two.srctrace",
        },
    )


def test_only_traces() -> None:
    thistest.log("== test_only_traces ==")
    xcov_args = instr_and_run_zero_one()

    # Commpute the coverage using the three trace files
    xcov(xcov_args + ["-axml", "traces/"])

    # Check the XML report against the expected result. We expect source traces
    # origins only, never "c.ckpt".
    expected_files = [
        "traces/one.srctrace",
        "traces/two.srctrace",
        "traces/zero.srctrace",
    ]
    main_origins = {
        "main_0.adb": "traces/zero.srctrace",
        "main_1.adb": "traces/one.srctrace",
        "main_2.adb": "traces/two.srctrace",
    }
    check_xml("only_traces.expected", expected_files, main_origins)

    # Input order may change IDs, but each must still resolve to the same file.
    xcov(xcov_args + ["-axml"] + list(reversed(expected_files)))
    check_xml("only_traces.expected", expected_files, main_origins)
    check_scaling(xcov_args)


# Run the tests

test_only_traces()
delete_traces()
test_with_checkpoint()
thistest.result()
