import glob
import os
from shutil import which
import sys

from e3.platform import Platform

gnatcov_src_dir = os.path.join(
    os.path.dirname(which("gnatcov" + Platform.get().os.exeext)),
    "..",
    "share",
    "gnatcoverage",
    "gnatcov_rts",
)
ccg_rts_dir = os.path.join(
    os.path.dirname(os.path.abspath(__file__)), "ccg_gnatcov_rts"
)

label = sys.argv[1]

lines = []

lines.append(f"# {label}")
for rts_dir in [gnatcov_src_dir, ccg_rts_dir]:
    objfiles = glob.glob(os.path.join(rts_dir, "*.o"))
    if objfiles:
        lines.append(f"Object files in {rts_dir}:")
        for filename in objfiles:
            lines.append(f"* {filename}")
    else:
        lines.append(f"No object file in {rts_dir}")

lines.append("")
content = "".join("\n" + line for line in lines)
with open(os.environ["CCG_DEBUG_FILE"], "a") as f:
    f.write(content)
