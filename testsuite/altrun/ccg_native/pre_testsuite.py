"""
Setup everything needed to be able to compile an instrumented executable with
CCG. This includes as custom version of gnatcov_rts, building the Ada runtime
for the proper platform, and pre-building libsupport.
"""

import json
import os
from pathlib import Path
from shutil import which

from e3.fs import rm, cp, mkdir, mv
from e3.os.process import Run
from e3.platform import Platform

altrun_dir = os.path.dirname(os.path.realpath(__file__))

custom_rts_dir = os.path.join(altrun_dir, "ccg_gnatcov_rts")
custom_rts_prj = os.path.join(custom_rts_dir, "gnatcov_rts.gpr")

exeext = Platform.get().os.exeext

gnatcov = which("gnatcov" + exeext)
base_rts_dir = os.path.join(
    os.path.dirname(gnatcov), "..", "share", "gnatcoverage", "gnatcov_rts"
)


def run(cmd, what):
    """
    Run the command represented by cmd in a subprocess, appending the
    executable extension for the platform to the first element of cmd. If the
    exit status is not success and ignore_failure is True, print
    f"{what} failed" followed by the command output on the standard output and
    exit with an error. Otherwise, return the output of the command execution.
    """
    cmd[0] = cmd[0] + exeext
    p = Run(cmd)
    if p.status != 0:
        print(f"{what} failed:")
        print(p.command_line_image())
        print(p.out)
        exit(1)
    return p.out


def amend_file(filename, substs):
    """
    Given a list of (pattern, subst) tuple of strings, search for each line, if
    some pattern is present in the line (string comparison), and replace the
    whole line with the corresponding substitution string. Otherwise, leave the
    line unchanged.
    The substitution string corresponding to the first matching pattern is
    applied, then processing moves on to the next line in the source file.
    """
    source = filename + ".source"
    mv(filename, source)
    found = False
    with open(source, "r") as src:
        with open(filename, "w") as dest:
            for line in src.readlines():
                found = False
                for pat, sub in substs:
                    if pat in line:
                        found = True
                        dest.write(sub + "\n")
                        break
                if not found:
                    dest.write(line)
    rm(source)


def prepare_rts():
    rm(custom_rts_dir, recursive=True)
    mkdir(custom_rts_dir)

    # Copy the runtime and remove obj and lib dirs if they exist
    cp(os.path.join(base_rts_dir, "*"), custom_rts_dir, recursive=True)
    rm(os.path.join(custom_rts_dir, "obj-*"), recursive=True)
    rm(os.path.join(custom_rts_dir, "lib-*"), recursive=True)

    # Remove gnatcov_rts_c-base_io*.c as it would interfere with the symbols
    # defined in gnatcov_rts-base_io.adb, and the C files that should not
    # be compiled when the RTS profile is "embedded".
    rm(os.path.join(custom_rts_dir, "gnatcov_rts_c-base_io*.c"))
    rm(os.path.join(custom_rts_dir, "gnatcov_rts_c-os_interface*"))
    rm(os.path.join(custom_rts_dir, "gnatcov_rts_c-traces-output-files*"))

    # Amend gnatcov_rts-base_io.adb to remove the dependency on GNAT.IO (it is
    # not available in the CCG runtime) and to directly use the libc instead.
    amend_file(
        os.path.join(custom_rts_dir, "gnatcov_rts-base_io.adb"),
        substs=[
            ("with GNAT.IO;", ""),
            (
                "pragma Warnings (On);",
                "      pragma Warnings (On);"
                "\n      function Putchar (C : Integer) return Integer;"
                "\n      pragma Import (C, Putchar);"
                "\n      Ignored : Integer;",
            ),
            (
                "GNAT.IO.Put (Str);",
                "      for C of Str loop"
                "\n         Ignored := Putchar (Character'Pos (C));"
                "\n      end loop;",
            ),
        ],
    )

    # Pre-build gnatcov_rts
    run(
        [
            "gprbuild",
            "--target=c",
            "-P",
            custom_rts_prj,
            "-XGNATCOV_RTS_FOR_CCG=true",
            "-XGNATCOV_RTS_RTS_PROFILE=embedded",
            "-cargs",
            f"-mtriple={Platform.get().triplet}",
        ],
        what="Custom gnatcov rts build",
    )

    # Flag it as externally built
    amend_file(
        custom_rts_prj,
        [
            (
                "end GNATcov_RTS;",
                'for Externally_Built use "True"; end GNATcov_RTS;',
            )
        ],
    )


# Prepare custom rts
prepare_rts()

# Re-build the lib support with the correct target:
# First, clean any previous libsupport build
libsupport_home = os.path.join("..", "..", "support")
run(
    ["make", "-C", libsupport_home, "-f", "Makefile.libsupport", "clean"],
    what="libsupport cleaning",
)

rm(os.path.join(libsupport_home, "silent_last_chance-*"))

# Then build the libsupport with the correct configuration and cflags
run(
    [
        "make",
        "--debug",
        "-C",
        libsupport_home,
        "-f",
        "Makefile.libsupport",
        "TARGET=c",
        f"LIBSUPPORT_CFLAGS=-mtriple={Platform.get().triplet}",
        "SILENT_LCH=exit",
    ],
    what="build of libsupport",
)

# Build the Ada runtime for the correct target

# First, determine where the runtime sources are located
prj_str = run(
    [
        "gprinspect",
        "-P",
        custom_rts_prj,
        "-XGNATCOV_RTS_FOR_CCG=true",
        "-XGNATCOV_RTS_RTS_PROFILE=embedded",
        "--target=c",
        "--display=json-compact",
    ],
    what="gprinspect invocation",
)
try:
    prj_info = json.loads(prj_str)
except Exception:
    print("failed parsing project info:")
    print(prj_str)
    exit(1)

adainclude_dir = None
for path in prj_info["tree"]["source-search-paths"]:
    if "adainclude" in path:
        adainclude_dir = path
        break
if not adainclude_dir:
    print("could no find adainclude directory from gprinspect output")
    print(prj_str)
    exit(1)

# Copy the sources in the altrun directory, in order not to modify the CCG
# build space.
local_rt = os.path.join(altrun_dir, "adainclude")
rm(local_rt, recursive=True)
cp(adainclude_dir, local_rt, recursive=True)

# Transpile the runtime to C
run(
    [
        "gprbuild",
        "-P",
        os.path.join(altrun_dir, "libgnat.gpr"),
        "--target=c",
        "-cargs",
        f"-mtriple={Platform.get().triplet}",
        "-gnatpg",
    ],
    what="compilation of the Ada runtime",
)

# Compile all the C files
all_rt_src = [str(pth.absolute()) for pth in Path(local_rt).glob("*.c")]
all_obj_files = []
for f in all_rt_src:
    all_obj_files.append(f[:-2] + ".o")
    run(
        ["gcc", "-c", f, "-o", all_obj_files[-1], "-I", adainclude_dir],
        what=f"compilation of {f}",
    )

# Create the library per se.
run(
    ["ar", "rcs", os.path.join(local_rt, "libgnat.a")] + all_obj_files,
    what="archival of the Ada runtime",
)
