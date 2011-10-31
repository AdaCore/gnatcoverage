#!/usr/bin/env python

import sys
from xcov import XCovReport

def show_violations():
    r = XCovReport(sys.argv[1])
    def pp(source):
        output = source.pp_violations(r.get_levels())
        if output:
            print output
    r.visitor(pp)

if __name__ == '__main__':
    show_violations()
