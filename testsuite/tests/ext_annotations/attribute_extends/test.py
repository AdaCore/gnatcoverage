"""
Check that Coverage'External_Annotations names files relatively to the project
that defines the attribute.

An extending project inherits the attribute, but the names it holds were
written in the extended project, so resolving them against the extending
project designates a file in the wrong directory -- and finds nothing there,
since a designated file that does not exist is not an error.
"""

import json
import os
import os.path

from SCOV.minicheck import xcov
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import Exempt_On, Exempt_Region, gprfor, xcov_annotate

tmp = Wdir("tmp_")

os.makedirs("base/obj", exist_ok=True)
os.makedirs("ext/obj", exist_ok=True)

base_gpr = gprfor(
    mains=None,
    prjid="my_base",
    srcdirs=["../../src"],
    cwd="base",
    extra="""
   package Coverage is
      for External_Annotations use "annotations.toml";
   end Coverage;
""",
)

# Written by hand: gprfor always extends the testsuite's own base project, and
# a project can only extend one.
with open("ext/ext.gpr", "w") as f:
    f.write(
        """project Ext extends "../base/my_base.gpr" is
   for Object_Dir use "obj";
end Ext;
"""
    )

base_sw = GPRswitches(root_project=base_gpr)
ext_sw = GPRswitches(root_project="ext/ext.gpr")

base_annotations = os.path.abspath("base/annotations.toml")

# Create the annotation through the project that defines the attribute.
#
# Where the annotation goes is what is under test, so no annot_out_file is
# passed: the project has to be what decides.
xcov_annotate(
    Exempt_Region("../src/pkg.adb", "4:7", "6:13", "defensive code"),
    gprsw=base_sw,
    extra_args=["--annotation-id=inherited"],
)

thistest.fail_if(
    not os.path.exists(base_annotations),
    "the annotation file should sit next to the project defining the"
    " attribute",
)

# Reading through the extending project must find that very same file.
xcov(["show-annotations"] + ext_sw.cov_switches, out="show.log")
thistest.fail_if_no_match(
    "annotation seen from the extending project",
    r"(?s).*- 4:7 - 6:13; id: inherited; kind: Exempt_Region;.*",
    contents_of("show.log"),
)

# And must report it as the file in effect: that list is what an IDE watches
# for changes, and it is the only place the resolved path is published.
xcov(
    ["show-annotations"]
    + ext_sw.cov_switches
    + ["--format=json", "--output=show.json"]
)
thistest.fail_if_not_equal(
    "annotation files seen from the extending project",
    [base_annotations],
    json.loads(contents_of("show.json"))["annotation_files"],
)

# The same resolution decides where an edit goes, so creating an annotation
# through the extending project must update the file the extended one names,
# rather than start a second one next to the extending project.
xcov_annotate(
    Exempt_On(
        "../src/pkg.adb", "4:7", None, "added through the extending project"
    ),
    gprsw=ext_sw,
    extra_args=["--annotation-id=through_ext"],
)

thistest.fail_if(
    os.path.exists("ext/annotations.toml"),
    "add-annotation should not create an annotation file next to the"
    " extending project",
)
thistest.fail_if_no_match(
    "annotation added through the extending project",
    r"(?s).*\[through_ext\].*",
    contents_of(base_annotations),
)

# And so must a deletion.
xcov(
    ["delete-annotation"]
    + ext_sw.cov_switches
    + ["--annotation-id=through_ext"],
)
thistest.fail_if(
    "through_ext" in contents_of(base_annotations),
    "delete-annotation should have updated the file the extended project"
    " designates",
)

thistest.result()
