import os.path

from SUITE.cutils import Wdir, lines_of, indent
from SUITE.tutils import thistest, xcov

# main.trace.asset contains an absolute path to the corresponding executable.
# As it was produced on some specific machine, this absolute path is not valid
# everywhere, so help gnatcov locate it anyway with the --exec option.

exec_path = os.path.abspath("main")
tmp = Wdir("tmp_")
xcov(
    [
        "coverage",
        "--level=branch",
        "--annotate=asm",
        "--routines=@../routines.txt",
        "--exec={}".format(exec_path),
        "../main.trace.asset",
    ],
    out="asm-report.txt",
    tolerate_messages="executable file .* does not seem to match trace file",
)
lines = lines_of("asm-report.txt")

baseline = """\
26 instructions analyzed:
  14 covered
  12 not executed
4 conditional branches analyzed:
  1 fully covered
  2 partially covered
  1 not executed"""
result = "\n".join(line.rstrip() for line in lines[-7:])

thistest.fail_if(
    baseline != result,
    'Unexpected output for "gnatcov coverage". Expected:\n'
    "{}\n"
    "but got:\n"
    "{}".format(indent(baseline), indent(result)),
)

thistest.result()
