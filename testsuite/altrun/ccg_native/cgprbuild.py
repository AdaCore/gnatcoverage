#!/usr/bin/env python3

import argparse
import json
import os
import pathlib

from e3.os.process import Run
from e3.platform import Platform


def parse_scenario(arg: str):
    """
    Helper to parse -XVAR[=VAL] command line options.
    If VAL is not specified in the argument, consider this an ill formed
    argument and ignore it.
    """
    return arg.split("=", maxsplit=1) if "=" in arg else None


def get_attr(obj, attr_name, idx=0):
    """
    Get the value of the attribute named attr_name, at index idx if relevant.
    If no such attribute exists, returns None.
    """
    attrs = obj["projects"][0]["attributes"]
    for attr_ob in attrs:
        if attr_ob["name"] == attr_name:
            if attr_ob["kind"] == "single":
                return attr_ob["value"]
            else:
                return attr_ob["values"][idx]
    return None


def run(cmd, what):
    """
    Run the command represented by cmd in a subprocess, appending the
    executable extension for the platform to the first element of cmd. If the
    exit status is not success and ignore_failure is True, print
    f"{what} failed" followed by the command output on the standard output and
    exit with an error. Otherwise, return the output of the command execution.
    """
    cmd[0] = cmd[0] + Platform.get().os.exeext
    p = Run(cmd)
    if p.status != 0:
        print(f"{what} failed:")
        print(p.command_line_image())
        print(p.out)
        exit(1)
    return p.out


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("-P", dest="project")
    parser.add_argument("--config", dest="config")
    parser.add_argument("--target", dest="target")
    parser.add_argument("--RTS", dest="rts")
    parser.add_argument("--src-subdirs", dest="src_subdirs")
    parser.add_argument("--implicit-with", dest="implicit_with")
    parser.add_argument(
        "-X", dest="scenario", action="append", type=parse_scenario,
    )
    parser.add_argument("-v", dest="verbose", action="store_true")

    args, extra = parser.parse_known_args()

    altrun_dir = os.path.dirname(os.path.realpath(__file__))

    # Substitute the "--implicit-with=gnatcov_rts.gpr" to point it toward our
    # own custom RTS.
    ccg_gnatcov_rts = os.path.join(
        altrun_dir, "ccg_gnatcov_rts", "gnatcov_rts.gpr",
    )
    custom_rts_dir = None
    if args.implicit_with and "gnatcov_rts" in args.implicit_with:
        custom_rts_dir = os.path.dirname(ccg_gnatcov_rts)
        args.implicit_with = ccg_gnatcov_rts

    # Build common args for all gpr<tools> commands
    common_args = ["--target=c"]
    common_args += ["-P", args.project]
    if args.src_subdirs:
        common_args += [f"--src-subdirs={args.src_subdirs}"]
    if args.implicit_with:
        common_args += [f"--implicit-with={args.implicit_with}"]
    for key, val in args.scenario:
        common_args += [f"-X{key}={val}"]

    # Add specific scenario for our custom version of gnatcov rts:
    common_args.append("-XGNATCOV_RTS_RTS_PROFILE=embedded")
    common_args.append("-XGNATCOV_RTS_FOR_CCG=true")

    if "-cargs" in extra:
        cargs_index = extra.index("-cargs")
        extra.insert(cargs_index + 1, f"-mtriple={Platform.get().triplet}")
    else:
        extra += ["-cargs", f"-mtriple={Platform.get().triplet}"]

    # Generate C files
    gprbuild_args = ["gprbuild"] + common_args + extra
    run(gprbuild_args, "gprbuild invocation")

    # Gather information about the project structure, we need the name of the
    # main (to generate an executable of the same name), the list of object
    # directories, the root directory of the project (as all other directories
    # are relatives to this), and the directory in which to place the
    # executable.
    gprinspect_args = (
        ["gprinspect"]
        + common_args
        + ["--attributes", "--display=json-compact"]
    )
    prj_txt = run(gprinspect_args, "gprinspect invocation")
    prj = json.loads(prj_txt)

    # Assume single main per project file
    main_name = os.path.basename(get_attr(prj, "Main")).split(".")[0]
    prj_dir   = get_attr(prj, "Project_Dir")
    exec_dir  = os.path.join(prj_dir, get_attr(prj, "Exec_Dir"))
    obj_dir   = os.path.join(prj_dir, get_attr(prj, "Object_Dir"))

    # Get the list of all the generated C files that need to be compiled. gprls
    # will return a list of ".o" files, so replace the extension to ".c"
    gprls_args = ["gprls"] + common_args + ["-o", "-U"]

    c_files = run(gprls_args, "gprls invocation").splitlines()
    c_files = [f for f in c_files if f[-2:] == ".c"]
    # Add the generated binder file in order to be able to link the executable
    c_files.append(os.path.join(obj_dir, "b__" + main_name + ".c"))

    # Hack: given the characteristics passed to the runtime (light runtime),
    # it will always add a "last_chance_dumper" unit which defines the symbol
    # __lch_enter. This symbol is also defined in the Libsupport unit, as part
    # of the Libsupport library. This normally is a static library, and thus
    # the object for __lch_enter should not be pulled during the link, but with
    # ccg we don't build any libraries. as such we must exclude libsupport.c
    # from the gcc invocation to avoid having multiple definitions of
    # __lch_enter.
    c_files = [
        filename
        for filename in c_files
        if os.path.basename(filename) != "libsupport.c"
    ]

    gcc_args = ["gcc"]

    if "-g" in extra:
        gcc_args += ["-g"]

    gcc_args += c_files

    # Add the list of native C files from gnatcov_rts: the project was modified
    # to not include C as a language (to avoid gprbuild issues), but these are
    # still required for a successful build.
    if custom_rts_dir:
        rts_dir = pathlib.Path(custom_rts_dir)
        gcc_args += [
            str(pth.absolute()) for pth in rts_dir.glob("gnatcov_rts_c*")
        ]

    # Add the runtime library in the mix. The pre-testsuite script compile the
    # library and does not bother to create a separate adalib directory for the
    # compilation results
    gcc_args += [os.path.join(altrun_dir, "adainclude", "libgnat.a")]

    # Compile the executable
    gcc_args += [
        "-o",
        os.path.join(exec_dir, main_name + Platform.get().os.exeext),
    ]
    run(gcc_args, "gcc invocation")


if __name__ == "__main__":
    main()
