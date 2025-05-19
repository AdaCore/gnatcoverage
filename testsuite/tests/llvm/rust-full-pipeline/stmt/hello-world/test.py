"""
Ensure a simple hello-world program is correctly covered
"""

from SCOV.minicheck import check_xcov_content
from SUITE.context import thistest
from SUITE.cutils import exists, Wdir
from SUITE.tutils import run_and_log, xcov

_tmp = Wdir("tmp_")

PROFRAW_FILE = "out.profraw"
EXEC_FILE = "target/debug/hello-world"

p = run_and_log(
    ["cargo", "build", "--manifest-path=../Cargo.toml"],
    env={"RUSTFLAGS": "-Cinstrument-coverage", "CARGO_TARGET_DIR": "./target"},
    ignore_environ=False,
)
thistest.fail_if(p.status != 0, f"cargo build failed: {p.err}")

p = run_and_log([EXEC_FILE], env={"LLVM_PROFILE_FILE": PROFRAW_FILE})
thistest.fail_if(p.status != 0, f"exe failed: {p.err}")

thistest.fail_if(
    not exists(PROFRAW_FILE), f"trace file '{PROFRAW_FILE}' not found"
)

xcov(["coverage", "--level=stmt", "-axcov", "--exec", EXEC_FILE, PROFRAW_FILE])

check_xcov_content("main.rs.xcov", {"+": {5, 6, 7}, "-": {1, 2, 3}})

thistest.result()
