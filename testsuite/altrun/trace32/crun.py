#!/usr/bin/env python

# Script that the suite engine will call instead of "gnatcov run" to execute
# programs, produce an execution trace on Lautherbach Trace32 tool, and
# convert them to a format that "gnatcov coverage" can process later on.

# This should return/exit with code 0 in case of success, with another value
# otherwise. More generally This needs to be ready to honor the "gnatcov run"
# interface, at least the subset of arguments that the testsuite driver might
# use for your target configuration. Simplest is to error out on unrecognized
# options.

import t32api
import sys
import optparse
import os
import gnatpython.ex

altrun_dir_path = os.path.dirname(os.path.realpath(__file__))


# ---------
# -- log --
# ---------
def log(str):
    print "trace32/crun.py:" + str


# -------------
# -- to_list --
# -------------
def to_list(blob):
    """Turn input BLOB into a list if it isn't already. Handle None
    and whitespace separated strings. Return empty list otherwise."""

    return (
        blob if isinstance(blob, list)
        else blob.split() if isinstance(blob, str)
        else []
        )


# -----------------
# -- contents_of --
# -----------------
def contents_of(filename):
    """Return contents of file FILENAME"""
    with open(filename) as fd:
        return fd.read()


# --------
# -- do --
# --------
def do(command):
    """Execute COMMAND. Abort and dump output on failure. Return output
    otherwise."""

    ofile = "cmd_.out"
    p = gnatpython.ex.Run(to_list(command), output=ofile)

    if p.status != 0:
        log("command '%s' failed" % command)
        log("Output was:\n" + contents_of(ofile))

    return contents_of(ofile)


class Runner:

    def __init__(self):

        log("============== CRUN FOR TRACE32 ===================")
        self.parse_command_line()
        self.run_with_trace32()
        self.run_gnatcov_convert()

    def parse_command_line(self):
        """Parse this script's command line."""

        op = optparse.OptionParser()

        # --level and --target are expected to always be there:

        op.add_option(
            "--level", dest="covlevel", default=None
            )
        op.add_option(
            "--target", dest="target", default=None
            )

        # For source coverage tests not using project files:

        op.add_option(
            "--scos", dest="scos", default=None
            )

        # For tests using project files

        op.add_option(
            "-P", dest="gpr", default=None
            )
        op.add_option(
            "--recursive", dest="recurse", default=False, action='store_true'
            )
        op.add_option(
            "--projects", dest="projects", default=None
            )
        op.add_option(
            "--units", dest="units", default=None
            )

        # Then a few optional items

        op.add_option(
            "-o", dest="ofile", default=None
            )

        (self.options, self.args) = op.parse_args()

    def run_command_line(self):
        """Compute a list of command line arguments to pass to
        gnatcov run from what we have received."""

        args = ["--level=%s" % self.options.covlevel]

        if self.options.target:
            args.append("--target=%s" % self.options.target)

        if self.options.scos:
            args.append("--scos=%s" % self.options.scos)

        if self.options.gpr:
            args.append("-P=%s" % self.options.gpr)
        if self.options.gpr:
            args.append("--projects=%s" % self.options.projects)
        if self.options.gpr:
            args.append("--units=%s" % self.options.units)
        if self.options.recurse:
            args.append("--recursive")

        if self.options.ofile:
            args.append("-o %s" % self.options.ofile)

        return args

    def convert_command_line(self):
        """Compute a list of command line arguments to pass to
        gnatcov run from what we have received."""

        args = ["--level=%s" % self.options.covlevel]

        args.append("--exec=%s" % self.get_executable_filename())

        args.append("--input=%s" % self.get_t32_trace_filename())

        args.append("--trace-source=Trace32-Branchflow")

        if self.options.target:
            args.append("--target=%s" % self.options.target)

        if self.options.scos:
            args.append("--scos=%s" % self.options.scos)

        if self.options.gpr:
            args.append("-P=%s" % self.options.gpr)
        if self.options.gpr:
            args.append("--projects=%s" % self.options.projects)
        if self.options.gpr:
            args.append("--units=%s" % self.options.units)
        if self.options.recurse:
            args.append("--recursive")

        args.append("-o %s" % self.get_gnatcov_trace_filename())

        return args

    def get_executable_filename(self):
        return self.args[0]

    def get_gnatcov_trace_filename(self):
        if self.options.ofile:
            return self.options.ofile
        else:
            return os.path.basename(self.get_executable_filename()) + ".trace"

    def get_t32_trace_filename(self):
        return self.get_gnatcov_trace_filename() + '.t32_branchflow'

    def run_with_trace32(self):
        print "=============== RUN ON TRACE32 ==================="
        if not self.args:
            log("trace32: missing EXE for run")
            sys.exit(1)
        log("Executable is: " + str(self.args))

        t32api.connect()
        t32api.basic_setup()
        t32api.init_trace_stm32f7()

        t32api.load_executable(self.get_executable_filename())
        t32api.set_breakpoint("__gnat_last_chance_handler")
        t32api.run_until("_exit", timeout_sec=500)
        t32api.export_trace(os.path.abspath(self.get_t32_trace_filename()))

        # TODO: Check if the execution ended in __gnat_last_chance_handler and
        # give that info to the testsuite.
        # Qualif/Appendix/Testsuite/Selftest/assert-failure

        if os.path.isfile(self.get_t32_trace_filename()):
            log("A trace file was produced...")
            self.run_gnatcov_convert()
        else:
            log("Trace32 crun: Not trace file produced...")
            sys.exit(1)
        print "=================================================="

    def run_gnatcov_convert(self):
        print "============== GNATCOV CONVERT ==================="
        do("gnatcov convert " + " ".join(self.convert_command_line()))
        print "=================================================="


runner = Runner()
