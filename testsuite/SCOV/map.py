# ****************************************************************************
# **                          MAP routines checker                          **
# ****************************************************************************

# This module exposes the MapChecker class, helper to compile a set of sources
# and check clean output of xcov map-routines on a list of alis and objects.

# ****************************************************************************

from SUITE.utils import thistest

import re
from SUITE.utils import match, do, compile, to_list, list_to_file
from SUITE.utils import SCOV_CARGS, COMMON_CARGS

# ==================
# ==  MapChecker  ==
# ==================
class MapChecker:

    def __init__(self, sources, options="",
                 objects=None, alis=None, ensure_dcscos=True):
        self.options = SCOV_CARGS +  " " + COMMON_CARGS + " " + options
        self.sources = to_list(sources)
        self.ensure_dcscos = ensure_dcscos

        # Infer default list of objects and alis from list of sources

        if objects != None:
            self.objects = to_list(objects)
        else:
            self.objects = ["%s.o" % source.split('.')[0]
                            for source in self.sources]

        if alis != None:
            self.alis = to_list(alis)
        else:
            self.alis = ["%s.ali" % source.split('.')[0]
                         for source in self.sources]

    def run(self):

        # Compile all the sources

        [compile (source, self.options) for source in self.sources]

        # If requested, check at least one non statement SCO in alis

        if self.ensure_dcscos:
            [thistest.fail_if (
                    not match ('^C[^S ]', ali, re.MULTILINE),
                    "couldn't find non-statement SCO in %s" % ali)
             for ali in self.alis]

        # Run xcov map-routines and check absence of errors

        mapoutput = do(
            "xcov map-routines -v --scos=@%s %s"
            % (list_to_file(self.alis), " ".join(self.objects)))

        maperrors = [str(m) for m in
                     re.findall("(\*\*\*|\!\!\!)(.*)", mapoutput)]

        thistest.log('\n'.join(maperrors))
        thistest.fail_if(
            maperrors, "expect no map-routines error for %s" % source)

