"""
Check that the cobertura.dtd standard validates the cobertura report that
gnatcov produces.
"""

import os

from lxml import etree

from SCOV.minicheck import build_run_and_coverage
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest

Wdir("tmp_")


def build_run_coverage_and_check(covlevel):
    build_run_and_coverage(
        gprsw=GPRswitches(root_project=gprfor(srcdirs=[".."],
                                              mains=["test.adb"])),
        covlevel=covlevel,
        mains=["test"],
        extra_coverage_args=["--annotate=cobertura"]
    )
    parser = etree.XMLParser(dtd_validation=True)
    etree.parse(os.path.join("obj", "cobertura.xml"), parser)


build_run_coverage_and_check("stmt+mcdc")

if thistest.options.trace_mode == "bin":
    build_run_coverage_and_check("insn")
    build_run_coverage_and_check("branch")

thistest.result()
