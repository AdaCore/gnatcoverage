from SUITE.context import thistest
from SUITE.cutils import Wdir, contents_of
from SUITE.tutils import exename_for, gprbuild, gprfor, xcov


wd = Wdir("tmp_")
gprbuild(gprfor(["foo.adb"], srcdirs=[".."]))

p = xcov(
    ["run", "-P", "does_not_exist.gpr", exename_for("foo")],
    out="gnatcov-run.out",
    err="gnatcov-run.err",
    register_failure=False,
)

thistest.fail_if(
    p.status != 1,
    "gnatcov returned status code {}, but 1 expected".format(p.status),
)
thistest.fail_if_not_equal(
    "'gnatcov run' output", "", contents_of("gnatcov-run.out")
)
thistest.fail_if_no_match(
    "'gnatcov run' error output",
    'does_not_exist\\.gpr: error: project file ".*[/\\\\]does_not_exist\\.gpr"'
    " not found\n"
    ".*gnatcov.*: Could not load the project file, aborting.\n$",
    contents_of("gnatcov-run.err"),
)

thistest.result()
