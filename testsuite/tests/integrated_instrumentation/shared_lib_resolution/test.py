"""
Check that the internal command "gnatcov dump-shared-lib-deps" can find DLLs in
various situations without emitting warnings.
"""

import os.path

from e3.fs import cp, mkdir

from SUITE.context import thistest
from SUITE.control import env
from SUITE.cutils import Wdir, lines_of
from SUITE.tutils import gprbuild, gprfor, xcov


Wdir("tmp_")

# Create a project tree for a single executable that imports multiple DLLs
dll_count = 3
for i in range(dll_count):
    with open(f"mylib_{i}.ads", "w") as f:
        f.write(
            f"""
            package Mylib_{i} is
               procedure P;
            end Mylib_{i};
            """
        )
    with open(f"mylib_{i}.adb", "w") as f:
        f.write(
            f"""
            package body Mylib_{i} is
               procedure P is
               begin
                  null;
               end P;
            end Mylib_{i};
            """
        )
    gprfor(
        prjid=f"mylib_{i}",
        mains=[],
        srcdirs=["."],
        objdir=f"obj/mylib_{i}",
        extra=f"""
            for Source_Files use ("mylib_{i}.ads", "mylib_{i}.adb");
            for Library_Name use "mylib_{i}";
            for Library_Dir use "lib/mylib_{i}";
            for Library_Kind use "relocatable";
        """,
    )

with open("main.adb", "w") as f:
    for i in range(dll_count):
        f.write(f"with Mylib_{i};\n")
    f.write("procedure Main is\nbegin\n")
    for i in range(dll_count):
        f.write(f"   Mylib_{i}.P;\n")
    f.write("end Main;")
p = gprfor(
    prjid="main",
    mains=["main.adb"],
    srcdirs=["."],
    objdir="obj/main",
    exedir="bin",
    deps=[f"mylib_{i}" for i in range(dll_count)],
    extra="""
        for Source_Files use ("main.adb");
    """,
)

gprbuild(p, gargs=["-XLIBRARY_TYPE=relocatable"])

# * libmylib_0.dll will stay out of reach (to check that we get a warning in
#   this case).
# * libmylib_1.dll will be made available through the PATH.
# * libmylib_2.dll will be made available through being near main.exe.

mkdir("in_path")
cp("lib/mylib_1/libmylib_1.dll", "in_path/")
env.add_search_path("PATH", os.path.abspath("in_path"))
cp("lib/mylib_2/libmylib_2.dll", "bin/")

# Run gnatcov's integrated DLLs resolver
filename = "deps.txt"
xcov(
    ["dump-shared-lib-deps", "bin/main.exe"],
    out=filename,
    tolerate_messages=".*",
)
lines = lines_of(filename)

dlls = set()
other_messages = []
for line in lines:
    if os.path.exists(line):
        dlls.add(os.path.realpath(line))
    else:
        other_messages.append(line)

# Check the absence of warnings
thistest.fail_if_not_equal(
    'messages from "gnatcov dump-shared-lib-deps"',
    "warning: Could not find library libmylib_0.dll. Add its directory to the"
    " PATH if this is an instrumented library.",
    "\n".join(other_messages),
)

# Check that mylib.dll was found
for found_lib in [
    "in_path/libmylib_1.dll",
    "bin/libmylib_2.dll",
]:
    expected = os.path.realpath(found_lib)
    thistest.fail_if(
        expected not in dlls,
        f"Could not find {expected} in listed DLLs:\n"
        + "\n".join(sorted(dlls)),
    )

thistest.result()
