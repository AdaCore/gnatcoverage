import re

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

out_content = contents_of("gnatcov-run.out")
err_content = contents_of("gnatcov-run.err")
expected_err = re.compile(
    "^does_not_exist.gpr is not a regular file\n"
    "[^\n]*gnatcov(\\.exe)?: Could not load the project file, aborting.\n$"
)

thistest.fail_if(
    p.status != 1,
    "gnatcov returned status code {}, but 1 expected".format(p.status),
)
thistest.fail_if(
    out_content, "gnatcov output not empty:\n" "\n{}\n".format(out_content)
)
thistest.fail_if(
    not expected_err.match(err_content),
    "gnatcov error output mismatch:\n"
    "Expected pattern:\n{!r}\n\n"
    "Got:\n{}".format(expected_err.pattern, err_content),
)

thistest.result()
