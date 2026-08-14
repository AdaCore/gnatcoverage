"""
Check the structured output of show-annotations --format=json.

The fields are what IDEs consume, so this checks the shape rather than a text
baseline: the annotation model, not the layout of the annotation file. In
particular the values that the two output formats normalise -- the 1-based
condition index, the suppression of a zero decision offset -- are pinned here,
since nothing else would notice them drifting apart.
"""

import json
import os
import os.path
import shutil

from SCOV.minicheck import xcov
from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.gprutils import GPRswitches
from SUITE.tutils import (
    Cov_Off,
    Dump_Buffers,
    Exempt_On,
    Exempt_Region,
    Ext_Annotation,
    Reset_Buffers,
    gprfor,
    xcov_annotate,
)

tmp = Wdir("tmp_")

# Work on a copy of the sources: one annotation below is invalidated by
# modifying the file it applies to.
shutil.copytree("../src", "src")

annotations = os.path.abspath("annotations.toml")

gpr = gprfor(
    mains=["main.adb"],
    srcdirs=["src"],
    extra="""
   package Coverage is
      for External_Annotations use "annotations.toml";
   end Coverage;
""",
)


sw = GPRswitches(root_project=gpr)


def add(annotation: Ext_Annotation, *extra: str) -> None:
    """
    Create an annotation, letting the project say where it goes.

    No annot_out_file is passed: the project attribute has to be what decides.
    """
    xcov_annotate(annotation, gprsw=sw, extra_args=list(extra))


def add_raw(*args: str) -> None:
    """
    Create an annotation from switches spelled out by the caller.

    For the decision kinds, which have no Ext_Annotation subclass.
    """
    xcov(["add-annotation"] + sw.cov_switches + list(args))


# One annotation of each shape, covering every kind-specific field: a region
# carrying a justification, a point carrying the buffer fields, and the
# decision kinds carrying the normalised ones.
add(
    Exempt_Region(
        "src/pkg.adb", "4:7", "6:13", "defensive; code\nsecond line"
    ),
    "--annotation-id=region",
)
add(
    Dump_Buffers(
        "src/main.adb",
        "5:7",
        None,
        insert_after=True,
        trace_prefix="myprefix",
    ),
    "--annotation-id=dump",
)
add_raw(
    "--kind=Exempt_Decision_Outcome",
    "--location=4:7",
    "--outcome=true",
    "--decision=2",
    "--justification=j",
    "--annotation-id=outcome",
    "src/pkg.adb",
)
add_raw(
    "--kind=Exempt_Decision_Condition",
    "--location=4:7",
    "--condition=1",
    "--justification=j",
    "--annotation-id=condition",
    "src/pkg.adb",
)
add_raw(
    "--kind=Manual_Decision_Evaluation",
    "--location=4:7",
    "--values=TFT",
    "--justification=j",
    "--annotation-id=manual",
    "src/pkg.adb",
)

# Cov_Off accepts a justification but does not require one. gnatcov emits the
# field all the same, empty -- which is what tells a client "no justification"
# apart from "this kind has no such field".
add(
    Cov_Off("src/pkg.adb", "4:7", None),
    "--justification=",
    "--annotation-id=covoff",
)

# A buffer kind without a trace prefix, to pin that insert_after is reported
# whatever its value while trace_prefix is left out entirely.
add(
    Reset_Buffers("src/pkg.adb", "4:7", None),
    "--annotation-id=reset",
)

# An annotation that will not survive an edit of its file: the absolute
# backend records a checksum, unlike the default one, which follows the
# construct it designates.
add(
    Exempt_On("src/pkg.adb", "4:7", None, "j"),
    "--annotation-id=doomed",
    "--ss_backend=absolute",
)

with open("src/pkg.adb", "a") as f:
    f.write("\n--  Appended, invalidating the absolute annotation.\n")

# --output gives the report a file of its own. That is what lets a consumer
# parse it: standard output also carries whatever gnatcov has to say, and a
# warning landing in the document would be a parse error, not a diagnostic.
xcov(
    ["show-annotations"]
    + sw.cov_switches
    + ["--format=json", "--output=show.json"],
    out="show-stdout.log",
)
thistest.fail_if(
    "annotation_files" in contents_of("show-stdout.log"),
    "--output should keep the report off standard output",
)

report = json.loads(contents_of("show.json"))

# The annotation files in effect are reported, so that a client can watch them
# without having to resolve the attribute itself.
thistest.fail_if_not_equal("code", "ok", report["code"])
thistest.fail_if_not_equal("message", "", report["message"])

thistest.fail_if_not_equal(
    "annotation_files", [annotations], report["annotation_files"]
)

thistest.fail_if_not_equal(
    "number of annotations", 8, len(report["annotations"])
)

by_id = {entry["id"]: entry for entry in report["annotations"]}
thistest.fail_if_not_equal(
    "reported annotations",
    {
        "condition",
        "covoff",
        "doomed",
        "dump",
        "manual",
        "outcome",
        "region",
        "reset",
    },
    set(by_id),
)

region = by_id["region"]
thistest.fail_if_not_equal("region kind", "Exempt_Region", region["kind"])
thistest.fail_if_not_equal("region stale", False, region["stale"])
thistest.fail_if_not_equal(
    "region file", os.path.abspath("src/pkg.adb"), region["file"]
)
thistest.fail_if_not_equal(
    "region location",
    {"start_line": 4, "start_column": 7, "end_line": 6, "end_column": 13},
    region["location"],
)

# A justification holding a semicolon and a newline is ambiguous in the text
# form, which is the main reason for this format to exist.
thistest.fail_if_not_equal(
    "region justification",
    "defensive; code\nsecond line",
    region["justification"],
)

dump = by_id["dump"]
thistest.fail_if_not_equal("dump kind", "Dump_Buffers", dump["kind"])
thistest.fail_if_not_equal("dump insert_after", True, dump["insert_after"])
thistest.fail_if_not_equal(
    "dump trace_prefix", "myprefix", dump["trace_prefix"]
)

# A buffer kind carries no justification field at all.
thistest.fail_if_not_equal(
    "dump justification", None, dump.get("justification")
)

# Whereas a kind that accepts one carries it even when it is empty.
thistest.fail_if_not_equal(
    "empty justification", "", by_id["covoff"]["justification"]
)

# insert_after is reported whether or not --annotate-after was passed, and a
# trace prefix only when one was given.
reset = by_id["reset"]
thistest.fail_if_not_equal("reset insert_after", False, reset["insert_after"])
thistest.fail_if(
    "trace_prefix" in reset,
    "no trace prefix was given, so none should be reported",
)

outcome = by_id["outcome"]
thistest.fail_if_not_equal("outcome", True, outcome["outcome"])
thistest.fail_if_not_equal("decision offset", 2, outcome["decision"])

# Condition indices are stored 0-based and reported 1-based, as in the text
# form: --condition=1 must come back as 1, not as the 0 held in the file.
condition = by_id["condition"]
thistest.fail_if_not_equal("condition index", 1, condition["condition"])
thistest.fail_if_no_match(
    "condition stored 0-based",
    r"(?s).*\ncondition = 0\n.*",
    contents_of(annotations),
)

# A zero decision offset is left out rather than reported as 0.
thistest.fail_if_not_equal(
    "absent decision offset", None, condition.get("decision")
)

thistest.fail_if_not_equal(
    "manual decision values", [True, False, True], by_id["manual"]["values"]
)

# A stale annotation has no location, and says why instead.
doomed = by_id["doomed"]
thistest.fail_if_not_equal("stale flag", True, doomed["stale"])
thistest.fail_if(
    "location" in doomed, "a stale annotation should carry no location"
)
thistest.fail_if(
    not doomed.get("diagnostic"),
    "a stale annotation should say why it no longer matches",
)
thistest.fail_if_not_equal(
    "stale file", os.path.abspath("src/pkg.adb"), doomed["file"]
)

# text is the default, and is still what an unadorned invocation prints.
xcov(["show-annotations"] + sw.cov_switches, out="show.txt")
thistest.fail_if_no_match(
    "default text output",
    r"(?s).*- 4:7 - 6:13; id: region; kind: Exempt_Region;.*",
    contents_of("show.txt"),
)

# The two formats agree on the normalised condition index.
thistest.fail_if_no_match(
    "condition reported 1-based in text too",
    r"(?s).*id: condition; kind: Exempt_Decision_Condition; Condition: 1;.*",
    contents_of("show.txt"),
)

xcov(
    ["show-annotations"] + sw.cov_switches + ["--format=text"],
    out="show-text.txt",
)
thistest.fail_if_not_equal(
    "--format=text matches the default",
    contents_of("show.txt"),
    contents_of("show-text.txt"),
)

# --output is about where the report goes, not about its format, so it applies
# to the text one as well.
xcov(["show-annotations"] + sw.cov_switches + ["--output=show-out.txt"])
thistest.fail_if_not_equal(
    "text report written to --output",
    contents_of("show.txt"),
    contents_of("show-out.txt"),
)
# An unknown format is rejected rather than silently ignored.
xcov(
    ["show-annotations"] + sw.cov_switches + ["--format=yaml"],
    out="bad-format.log",
    register_failure=False,
)
thistest.fail_if_no_match(
    "unknown format diagnostic",
    r"(?s).*Unknown output format.*",
    contents_of("bad-format.log"),
)

thistest.result()
