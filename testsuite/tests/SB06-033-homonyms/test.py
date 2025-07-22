"""
Check that "gnatcov coverage" does not crash when processing a program that
exposes two paths for the same source (once before and one after installation).

Note that in this testcase, both absolute paths come from debug info line
mapping (.debug_lines).
"""

import os.path

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import gprbuild, gprfor, gprinstall, xcov, xrun


# Build libfoo and install it in some prefix, then make the installed project
# available through the GPR_PROJECT_PATH environment variable.
tmp = Wdir("tmp_libfoo")
install_dir = os.path.abspath("install")
libfoo_gpr = gprfor(
    prjid="libfoo",
    mains=[],
    srcdirs=["../libfoo"],
    langs=["Ada"],
    extra="""
        for Library_Kind use "static";
        for Library_Name use "foo";
        for Library_Dir use "lib";
    """,
)
gprbuild(libfoo_gpr)
gprinstall(libfoo_gpr, gargs=[f"--prefix={install_dir}"])
os.environ["GPR_PROJECT_PATH"] = os.path.join(install_dir, "share", "gpr")
tmp.to_homedir()

# Now, in another directory (so that we are sure it is the installed libfoo
# that is used), build the main application, then generate a binary trace for
# it.
tmp = Wdir("tmp_app")
app_gpr = gprfor(
    prjid="app", mains=["main.adb"], srcdirs=["../app"], deps=["libfoo"]
)
gprbuild(app_gpr)
xrun("./main")

# The very goal of this testcase is to compute code coverage for a unit that
# belongs to a project installed with gprinstall, so we need to enable the
# processing of externally built projects.
log_file = "gnatcov-coverage.txt"
xcov(
    [
        "coverage",
        "--annotate=xcov",
        "--level=stmt",
        "-P",
        app_gpr,
        "--projects=app",
        "--externally-built-projects",
        "main.trace",
    ],
    out=log_file,
)
thistest.fail_if_no_match(
    '"gnatcov output" ({})'.format(log_file),
    "Warning: same base name for files:"
    "\r?\n  [^\n]+{}"
    "\r?\n  [^\n]+{}".format(
        os.path.join(
            "SB06-033-homonyms",
            "tmp_libfoo",
            "install",
            "include",
            "libfoo",
            "lib.adb",
        ),
        os.path.join("SB06-033-homonyms", "libfoo", "lib.adb"),
    ),
    contents_of(log_file),
)

thistest.result()
