"""
Check that the Coverage'External_Annotations project attribute designates
external annotation files, and that --external-annotations overrides it.

The attribute names a file that does not exist until the first annotation is
created, so this also checks that gnatcov does not report it as missing.
"""

import os.path

from SCOV.minicheck import xcov
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import Exempt_Region, gprfor, xcov_annotate

tmp = Wdir("tmp_")

annotations = os.path.abspath("annotations.toml")
other = os.path.abspath("other.toml")

# A project designating the annotation file through the attribute. The
# attribute is relative to the project directory, which is what a user would
# write, and which the tested code has to resolve.
gpr = gprfor(
    mains=["main.adb"],
    srcdirs=["../src"],
    extra="""
   package Coverage is
      for External_Annotations use "annotations.toml";
   end Coverage;
""",
)

sw = GPRswitches(root_project=gpr)

# Create the annotation designated by the attribute. add-annotation has to
# honour the attribute both to load the existing annotations and to know which
# file to update, so neither --external-annotations nor --output is passed.
# The attribute designates a file that does not exist yet, which is what a
# project looks like before its first annotation. gnatcov must not report it as
# missing, so the commands below are expected to be quiet about it.
xcov_annotate(
    Exempt_Region("../src/pkg.adb", "4:7", "6:13", "defensive code"),
    gprsw=sw,
    extra_args=["--annotation-id=from_attribute"],
    out="add.log",
)

# The annotation must have landed in the file the attribute designates.
thistest.fail_if(
    not os.path.exists(annotations),
    "add-annotation should have written to the file designated by the"
    " Coverage'External_Annotations attribute",
)

thistest.fail_if(
    "No such file" in contents_of("add.log"),
    "add-annotation should not report the not-yet-created annotation file as"
    " missing",
)

# show-annotations must find it without being told where it is.
xcov(
    ["show-annotations"] + sw.cov_switches + ["../src/pkg.adb"],
    out="show.log",
)

thistest.fail_if_no_match(
    "annotation from the project attribute",
    r"(?s).*from_attribute.*",
    contents_of("show.log"),
)

# An empty file on the command line takes precedence over the attribute, so
# nothing is reported any more.
with open(other, "w"):
    pass
xcov(
    ["show-annotations"]
    + sw.cov_switches
    + [f"--external-annotations={other}", "../src/pkg.adb"],
    out="override.log",
)
thistest.fail_if(
    "from_attribute" in contents_of("override.log"),
    "--external-annotations should override the project attribute",
)

# delete-annotation must honour the attribute as well.
xcov(
    ["delete-annotation"]
    + sw.cov_switches
    + ["--annotation-id=from_attribute"],
)
xcov(
    ["show-annotations"] + sw.cov_switches + ["../src/pkg.adb"],
    out="after-delete.log",
)
thistest.fail_if(
    "from_attribute" in contents_of("after-delete.log"),
    "the annotation should have been deleted",
)

# A project designating nothing is refused, and the wording matters: the VS
# Code integration matches on it to tell "the feature is off for this project"
# apart from a real failure, so that a project using no annotations does not
# show an error.
plain_sw = GPRswitches(
    root_project=gprfor(mains=["main.adb"], srcdirs=["../src"], prjid="plain")
)
p = xcov(
    ["show-annotations"] + plain_sw.cov_switches + ["../src/pkg.adb"],
    out="none.log",
    register_failure=False,
)

# The VS Code integration only looks at the message when the exit status is
# non-zero, so demoting this to a warning would break it silently.
thistest.fail_if(
    p.status == 0,
    "show-annotations should fail when nothing designates an annotation file",
)
thistest.fail_if_no_match(
    "diagnostic for a project designating no annotation file",
    r"(?s).*no external annotation file.*",
    contents_of("none.log"),
)

thistest.result()
