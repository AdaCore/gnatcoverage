"""
Regression test: check that gnatcov does not crash when generating a XML / HTML
report with missing sources. It used to crash when trying to compute scope
metrics.
"""

from e3.fs import mv, sync_tree

import os

from SCOV.minicheck import build_and_run
from SUITE.cutils import Wdir
from SUITE.gprutils import GPRswitches
from SUITE.tutils import gprfor, thistest, xcov

tmp = Wdir("tmp_")

sync_tree(os.path.join("..", "src"), "src")

xcov_args = build_and_run(
    gprsw=GPRswitches(
        root_project=gprfor(srcdirs=["src"], mains=["test.adb"])
    ),
    covlevel="stmt",
    mains=["test"],
    extra_coverage_args=[],
    trace_mode="src",
)

# Then hide the sources
os.mkdir("src_hidden")
mv(os.path.join("src", "pkg.adb"), "src_hidden")

# Try to generate a HTML, and an XML report
xcov(xcov_args + ["-axml"], tolerate_messages="warning: can't open")
xcov(xcov_args + ["-ahtml"], tolerate_messages="warning: can't open")

thistest.result()
