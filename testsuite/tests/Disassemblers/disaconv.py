#! /usr/bin/env python

import argparse
import re
import sys


# Command-line parsing

parser = argparse.ArgumentParser(
    description=(
        "Convert a objdump/gnatcov disassembly dump to a simple format"
    )
)
parser.add_argument(
    "-o",
    "--output",
    type=argparse.FileType("w"),
    default=sys.stdout,
    dest="output",
)
parser.add_argument(
    "input",
    type=argparse.FileType("r"),
    nargs="?",
    default=sys.stdin,
)


# Internal data structures


def section(match):
    return "Section"


def insn(match):
    addr = int(match.group("addr"), 16)
    return "Insn:   {:08x}".format(addr)


# Input matching

# These are the pattern generated by objdump:
OBJDUMP_SECTION = re.compile("^Disassembly of section (?P<name>.+):$")
OBJDUMP_INSN = re.compile(
    "^ *(?P<addr>[0-9a-f]+):\t[ 0-9a-f]+\t" "(?P<mnemonic>[^ ]*)"
)

# And those ones are generated by gnatcov disassemble:
GNATCOV_SECTION = re.compile("^Section")
GNATCOV_INSN = re.compile(
    "(?P<addr>[0-9a-f]+) -: (?: [0-9a-f]{2})+ {3,}" "(?P<mnemonic>[^ ]+)"
)


# Matching table: associate a pattern with an action.

MATCHING_TABLE = (
    (OBJDUMP_SECTION, section),
    (OBJDUMP_INSN, insn),
    (GNATCOV_SECTION, section),
    (GNATCOV_INSN, insn),
)


def disaconv(fin, fout):
    # For each line in the input file, try to find a matching pattern and write
    # the result of the associated action to the output file.
    for line in fin:
        for pattern, action in MATCHING_TABLE:
            m = pattern.match(line)
            if m:
                fout.write(action(m))
                fout.write("\n")


if __name__ == "__main__":
    args = parser.parse_args()
    disaconv(args.input, args.output)
