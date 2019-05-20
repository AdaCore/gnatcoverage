# ****************************************************************************
# **                          MAP routines checker                          **
# ****************************************************************************

# This module exposes the MapChecker class, helper to compile a set of sources
# and check clean output of xcov map-routines on a list of alis and objects.

# ****************************************************************************

import os
import re

from SUITE.cutils import Wdir, match, to_list, list_to_file
from SUITE.tutils import (
    XCOV, thistest,
    gprfor, gprbuild, exename_for,
    do, maybe_valgrind)

from SUITE.control import BUILDER

# ==================
# ==  MapChecker  ==
# ==================
class MapChecker:

    def __init__(self, sources, options="",
                 execs=None, alis=None, ensure_dcscos=True):

        self.sources = to_list(sources)
        self.options = options
        self.ensure_dcscos = ensure_dcscos

        # Infer default list of executables and alis from list of sources

        if execs != None:
            self.execs = to_list(execs)
        else:
            self.execs = [
                exename_for (source.split('.')[0])
                for source in self.sources
            ]

        if alis != None:
            self.alis = to_list(alis)
        else:
            self.alis = [os.path.join("obj", "%s.ali" % source.split('.')[0])
                         for source in self.sources]

    def run(self):
        tmp = Wdir('tmp_')

        # Compile all the sources.  This method will not work if there are
        # sources that are not in the "." directory, but since executabes are
        # processed next, there will be an error if not all sources are
        # compiled.

        project = gprfor(
            self.sources, srcdirs=[".."], main_cargs=self.options)
        gprbuild(project, gargs=["-bargs", "-z"])

        # If requested, check at least one non statement SCO in alis

        if self.ensure_dcscos:
            [thistest.fail_if (
                    not match ('^C[^S ]', ali, re.MULTILINE),
                    "couldn't find non-statement SCO in %s" % ali)
             for ali in self.alis]

        # Run xcov map-routines and check absence of errors

        mapoutput = do(maybe_valgrind([
            XCOV, 'map-routines', '-v',
            '--scos=@{}'.format(list_to_file(self.alis)),
        ] + self.execs))

        maperrors = [str(m) for m in
                     re.findall("(\*\*\*|\!\!\!)(.*)", mapoutput)]

        thistest.log('\n'.join(maperrors))
        thistest.fail_if(
            maperrors,
            "expect no map-routines error for %s" % ", ".join(self.sources))

        tmp.to_homedir()

