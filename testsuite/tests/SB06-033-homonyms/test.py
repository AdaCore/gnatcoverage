"""
Check that "gnatcov coverage" does not crash when processing a program that
exposes two paths for the same source (once before and one after installation).

Note that in this testcase, both absolute paths come from debug info line
mapping (.debug_lines).
"""

import os

from e3.fs import rm
from e3.os.process import Run

from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of


wd = Wdir("tmp_")
log_dir = os.getcwd()
wd.to_homedir()


def in_home(*args):
    return os.path.join(wd.homedir, *args)


def try_run(cmd, out_file):
    out_file = os.path.join(log_dir, out_file)
    p = Run(cmd, output=out_file)
    out = contents_of(out_file)
    thistest.fail_if(
        p.status != 0,
        "Unexpected failure.\n"
        "Command was:\n"
        "%s\n"
        "Output was:\n"
        "%s" % (" ".join(cmd), out),
    )
    return out


os.chdir(in_home("libfoo"))
rm("install", recursive=True)

try_run(["gprbuild", "-f", "-Plibfoo.gpr", "-p"], "gprbuild-libfoo.txt")
try_run(
    [
        "gprinstall",
        "-f",
        "-Plibfoo.gpr",
        "-p",
        "--prefix=install",
        "--project-subdir=gpr",
    ],
    "gprinstall.txt",
)

os.chdir(in_home("app"))

try_run(["gprbuild", "-f", "-Pdefault.gpr", "-p"], "gprbuild-app.txt")
try_run(["gnatcov", "run", "obj/main"], "gnatcov-run.txt")

# The very goal of this testcase is to compute code coverage for a unit that
# belongs to a project installed with gprinstall, so we need to enable the
# processing of externally built projects.
log_file = "gnatcov-coverage.txt"
log = try_run(
    [
        "gnatcov",
        "coverage",
        "--annotate=xcov",
        "--level=stmt",
        "-Pdefault",
        "--externally-built-projects",
        "main.trace",
    ],
    log_file,
)
thistest.fail_if_no_match(
    '"gnatcov output" ({})'.format(log_file),
    "Warning: same base name for files:"
    "\r?\n  [^\n]+{}"
    "\r?\n  [^\n]+{}".format(
        os.path.join(
            "SB06-033-homonyms",
            "libfoo",
            "install",
            "include",
            "libfoo",
            "lib.adb",
        ),
        os.path.join("SB06-033-homonyms", "libfoo", "lib.adb"),
    ),
    log,
)

thistest.result()
