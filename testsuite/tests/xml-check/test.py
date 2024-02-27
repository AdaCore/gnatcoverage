"""Sanity check our ability to produce XML reports."""

from xml.dom import minidom

from SCOV.minicheck import build_run_and_coverage
from SUITE.context import thistest
from SUITE.gprutils import GPRswitches
from SUITE.cutils import Wdir
from SUITE.tutils import gprfor


tmp = Wdir("tmp_")

build_run_and_coverage(
    gprsw=GPRswitches(
        root_project=gprfor("test_andthen_t.adb", srcdirs="../src")
    ),
    covlevel="stmt+mcdc",
    mains=["test_andthen_t"],
    extra_coverage_args=["--annotate=xml", "--output-dir=."],
    scos=["obj/andthen"],
)

xmldoc = minidom.parse("andthen.adb.xml")
print(xmldoc.toxml())

xmldoc = minidom.parse("index.xml")
print(xmldoc.toxml())

thistest.result()
