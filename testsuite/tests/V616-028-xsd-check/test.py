"""
Check that the distributed XSD validates different kinds of XML reports
produced by gnatcov.
"""

import os

from lxml import etree

from SCOV.minicheck import build_run_and_coverage
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest

Wdir("tmp_")


def check_xml(xsd_file, xml_file):
    xsd_doc = etree.parse(xsd_file)
    xsd = etree.XMLSchema(xsd_doc)
    xml = etree.parse(xml_file)
    xml.xinclude()
    try:
        xsd.assertValid(xml)
    except Exception as exc:
        thistest.failed(
            "The XSD does not validate {}: \n {}".format(
                os.path.abspath(xml_file), exc
            )
        )


# The XML Schema is generated in the output directory with the XML report
def xsd_file(output_dir):
    return os.path.join(output_dir, "gnatcov-xml-report.xsd")


def xml_file(output_dir):
    return os.path.join(output_dir, "index.xml")


def build_run_coverage_and_check(covlevel):
    build_run_and_coverage(
        gprsw=GPRswitches(
            root_project=gprfor(srcdirs=[".."], mains=["test.adb"])
        ),
        covlevel=covlevel,
        mains=["test"],
        extra_coverage_args=["--annotate=xml"],
    )
    check_xml(xsd_file("obj"), xml_file("obj"))


build_run_coverage_and_check("stmt+mcdc")

if thistest.options.trace_mode == "bin":
    build_run_coverage_and_check("insn")
    build_run_coverage_and_check("branch")

thistest.result()
