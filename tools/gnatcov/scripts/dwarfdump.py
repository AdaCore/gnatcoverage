#! /usr/bin/env python
# -*- coding: utf-8 -*-
# Usage: dwarfdump.py objdump -Dr <exe>
# Annotates the output of objdump with sloc info from exe

import re
import subprocess
import sys

import slocinfo


OBJDUMP_ADDR = re.compile("^ *([0-9a-f]+):")


def do_dump(sloc_info, cmd):
    # Start the subprocess to disassemble (most likely objdump -d)
    print("Executing: {}".format(cmd))
    print("")
    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE)
    outs, _ = proc.communicate()
    outs = outs.decode()

    last_slocs = []
    for line in outs.split("\n"):
        m = OBJDUMP_ADDR.match(line)
        if m:
            addr = int(m.group(1), 16)
            slocs = sloc_info.get(addr, [])
            if slocs != last_slocs:
                print("")
                for sloc in slocs:
                    print(">>> {}".format(slocinfo.format_sloc(sloc)))
                last_slocs = slocs
        sys.stdout.flush()
        print(line)


if __name__ == "__main__":
    import errno

    try:
        do_dump(slocinfo.get_sloc_info(sys.argv[-1]), sys.argv[1:])
    except IOError as e:
        if e.errno != errno.EPIPE:
            raise
