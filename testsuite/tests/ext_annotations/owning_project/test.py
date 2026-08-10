"""
Check which annotation file each command reads and writes when several
projects in the tree designate one.

An annotation belongs with the unit it applies to, so add-annotation writes to
the file designated by the project owning that unit, not to the root project's.
Reading, on the other hand, takes in the whole tree: a dependency's annotations
are relevant to whoever depends on it. Deleting then has to rewrite only the
file the annotation came from, rather than collapsing everything into one.
"""

import json
import os
import os.path

from SCOV.minicheck import xcov
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import Exempt_On, gprfor, xcov_annotate

tmp = Wdir("tmp_")

os.makedirs("lib/obj", exist_ok=True)
os.makedirs("app/obj", exist_ok=True)
os.makedirs("plain/obj", exist_ok=True)

lib_gpr = gprfor(
    mains=None,
    prjid="liba",
    srcdirs=["../../src/liba"],
    cwd="lib",
    extra="""
   package Coverage is
      for External_Annotations use "lib_annotations.toml";
   end Coverage;
""",
)

app_gpr = gprfor(
    mains=None,
    prjid="app",
    srcdirs=["../../src/app"],
    cwd="app",
    deps=[f"../{lib_gpr}"],
    extra="""
   package Coverage is
      for External_Annotations use "app_annotations.toml";
   end Coverage;
""",
)

# A project designating nothing, to check the diagnostic.
plain_gpr = gprfor(
    mains=None,
    prjid="plain",
    srcdirs=["../../src/liba"],
    cwd="plain",
)

app_sw = GPRswitches(root_project=app_gpr)
plain_sw = GPRswitches(root_project=plain_gpr)

lib_annotations = os.path.abspath("lib/lib_annotations.toml")
app_annotations = os.path.abspath("app/app_annotations.toml")

# Annotate a unit owned by the library, through the project that imports it.
# The annotation must land next to the library, which is what owns the unit.
#
# Which file is written is what is under test, so no annot_out_file is passed:
# the project tree has to be what decides.
xcov_annotate(
    Exempt_On("../src/liba/pkg.adb", "4:7", None, "owned by the library"),
    gprsw=app_sw,
    extra_args=["--annotation-id=in_lib"],
)

thistest.fail_if(
    not os.path.exists(lib_annotations),
    "the annotation should have gone to the file the owning project"
    " designates",
)
thistest.fail_if(
    os.path.exists(app_annotations),
    "the annotation should not have gone to the root project's file",
)

# Reading through the importing project sees the library's annotations: they
# apply to units it depends on.
xcov(
    ["show-annotations"]
    + app_sw.cov_switches
    + ["--format=json", "--output=show.json"]
)
report = json.loads(contents_of("show.json"))

# Both are reported: a file a project designates but has not created yet is
# still in effect, since that is where its first annotation will go.
thistest.fail_if_not_equal(
    "annotation files in effect",
    sorted([app_annotations, lib_annotations]),
    sorted(report["annotation_files"]),
)
thistest.fail_if_not_equal(
    "annotations seen from the importing project",
    ["in_lib"],
    [entry["id"] for entry in report["annotations"]],
)

# Give the importing project an annotation of its own, so that a later
# deletion has something to leave alone.
with open(app_annotations, "w") as f:
    f.write(
        """[untouched]
at_most_once = true
file = "nothing.adb"
kind = "absolute"
[untouched.matcher]
end_col = 1
end_line = 1
start_col = 1
start_line = 1
[[untouched.annotations]]
purpose = "xcov.exempt.on"
justification = "left alone"
"""
    )

# Deleting the library's annotation, through the importing project, must
# rewrite the library's file only. Before annotations carried the file they
# came from, this wrote the whole set back to one file, duplicating entries.
xcov(
    ["delete-annotation"] + app_sw.cov_switches + ["--annotation-id=in_lib"],
)

thistest.fail_if(
    "in_lib" in contents_of(lib_annotations),
    "the annotation should have been deleted from the library's file",
)
thistest.fail_if_no_match(
    "the importing project's own file is left alone",
    r"(?s).*\[untouched\].*",
    contents_of(app_annotations),
)
thistest.fail_if(
    "in_lib" in contents_of(app_annotations),
    "deleting should not have copied the library's annotations into the"
    " importing project's file",
)

# A project that designates no annotation file cannot be annotated: gnatcov
# says so and stops, rather than picking a file of its own accord.
p = xcov_annotate(
    Exempt_On("../src/liba/pkg.adb", "4:7", None, "j"),
    gprsw=plain_sw,
    extra_args=["--annotation-id=nowhere"],
    out="plain.log",
    register_failure=False,
)
thistest.fail_if(
    p.status == 0,
    "add-annotation should fail when the owning project designates no file",
)
thistest.fail_if_no_match(
    "diagnostic naming the owning project's missing attribute",
    r"(?s).*\(plain\) designates no Coverage'External_Annotations file.*",
    contents_of("plain.log"),
)

# The command line wins over the project: --output redirects the write.
xcov_annotate(
    Exempt_On("../src/liba/pkg.adb", "4:7", None, "j"),
    gprsw=plain_sw,
    annot_out_file="explicit.toml",
    extra_args=["--annotation-id=via_cli"],
)
thistest.fail_if(
    not os.path.exists("explicit.toml"),
    "--output should override the project attribute",
)

thistest.result()
