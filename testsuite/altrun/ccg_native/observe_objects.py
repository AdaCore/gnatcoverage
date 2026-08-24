import glob
import os
import sys

rts_dir = os.path.join(
    os.path.dirname(os.path.abspath(__file__)), "ccg_gnatcov_rts"
)

label = sys.argv[1]

lines = []

lines.append(f"# {label}")
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
