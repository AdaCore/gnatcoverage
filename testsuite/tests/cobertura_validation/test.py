"""
Check that creating Cobertura coverage reports works as expected.
"""

import os

from lxml import etree

from SCOV.minicheck import build_run_and_coverage
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest


tmp = Wdir("tmp_")

gprsw = gprsw=GPRswitches(
    root_project=gprfor(srcdirs=[".."], mains=["test.adb"])
)


def build_run_coverage_and_check(covlevel, output_opt=None, source_root=None):
    extra_args = ["--annotate=cobertura"]
    if source_root:
        extra_args += ["--source-root", source_root]

    filename = output_opt if output_opt else "cobertura.xml"
    if output_opt:
        extra_args += ["-o", output_opt]

    build_run_and_coverage(
        gprsw=gprsw,
        covlevel=covlevel,
        mains=["test"],
        extra_coverage_args=extra_args,
    )
    parser = etree.XMLParser(dtd_validation=True)
    return etree.parse(os.path.join("obj", filename), parser)


# For both source and object coverage, check that the output "cobertura.xml"
# file is valid according to the cobertura.dtd standard.
build_run_coverage_and_check("stmt+mcdc")
if thistest.options.trace_mode == "bin":
    build_run_coverage_and_check("insn", "cobertura-insn.xml")
    build_run_coverage_and_check("branch", "cobertura-branch.xml")


# Check that the --source-root option works as expected
for prefix in [tmp.homedir, tmp.homedir + "/"]:
    thistest.log(f"== Prefix: {prefix} ==")

    report = build_run_coverage_and_check("stmt", source_root=prefix)
    filenames = set()
    for elt in report.getroot().iter():
        if elt.tag == "class":
            filenames.add(elt.get("filename"))

    expected_filenames = {"lib.c", "pkg.adb", "pkg.ads", "test.adb"}
    thistest.fail_if_not_equal(
        "list of filenames in cobertura.xml",
        "\n".join(sorted(expected_filenames)),
        "\n".join(sorted(filenames)),
    )

thistest.result()
