import glob
import os
import sys

rts_dir = os.path.join(
    os.path.dirname(os.path.abspath(__file__)), "ccg_gnatcov_rts"
)

label = sys.argv[1]
with open(os.environ["CCG_DEBUG_FILE"], "a") as f:
    print("#", label, file=f)
    objfiles = glob.glob(os.path.join(rts_dir, "*.o"))
    if objfiles:
        print(f"Object files in {rts_dir}:", file=f)
        for filename in objfiles:
            print("* ", filename, file=f)
    else:
        print(f"No object file in {rts_dir}", file=f)
    print("", file=f)
