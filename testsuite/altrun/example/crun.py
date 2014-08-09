#!/usr/bin/env python

# Example script to be used as a testsuite.py --gnatcov-run argument, which
# the suite engine will call instead of "gnatcov run" to execute programs and
# produce an execution trace that "gnatcov coverage" can process later on.

# This should return/exit with code 0 in case of success, with another value
# otherwise. More generally This needs to be ready to honor the "gnatcov run"
# interface, at least the subset of arguments that the testsuite driver might
# use for your target configuration. Simplest is to error out on unrecognized
# options.

# The code below eventually resorts to a real "gnatcov run" invocation. While
# there is no real added value in doing that, this code is useful as an
# example of a possible way to achieve the goal of replacing gnatcov run in
# other situations. A typical case is one where the program execution is
# performed on a real target board, producing a some trace that needs to be
# converted to a format gnatcov can process.

import os
import optparse

import gnatpython.ex

class Runner:

    def __init__(self):

        self.parse_command_line()
        self.run()

    def parse_command_line(self):
        """Parse this script's command line."""

        op = optparse.OptionParser()

        # --level and --target are expected to always be there:

        op.add_option (
            "--level", dest="covlevel", default=None
            )
        op.add_option (
            "--target", dest="target", default=None
            )

        # For source coverage tests not using project files:

        op.add_option (
            "--scos", dest="scos", default=None
            )

        # For tests using project files

        op.add_option (
            "-P", dest="gpr", default=None
            )
        op.add_option (
            "--recursive", dest="recurse", default=False, action='store_true'
            )
        op.add_option (
            "--projects", dest="projects", default=None
            )
        op.add_option (
            "--units", dest="units", default=None
            )

        # Then a few optional items

        op.add_option (
            "-o", dest="ofile", default=None
            )

        (self.options, self.args) = op.parse_args()

    def run_command_line(self):
        """Compute a list of command line arguments to pass to
        gnatcov run from what we have received."""

        args = ["--level=%s" % self.options.covlevel]

        if self.options.target:
            args.append ("--target=%s" % self.options.target)

        if self.options.scos:
            args.append ("--scos=%s" % self.options.scos)

        if self.options.gpr:
            args.append ("-P=%s" % self.options.gpr)
        if self.options.gpr:
            args.append ("--projects=%s" % self.options.projects)
        if self.options.gpr:
            args.append ("--units=%s" % self.options.units)
        if self.options.recurse:
            args.append ("--recursive")

        if self.options.ofile:
            args.append ("-o %s" % self.options.ofile)


        return args

    def run(self):

        print "============== GNATCOV RUN ==================="

        if not self.args:
            print "gnatcov: missing EXE for run"
            os.exit(1)

        gnatpython.ex.Run (
            ['gnatcov', 'run'] + self.run_command_line() + self.args)

        print "=============================================="

runner = Runner()
