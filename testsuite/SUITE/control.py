# ***************************************************************************
# **                             TEST SUITE CONTROL                        **
# ***************************************************************************

from gnatpython.env import Env

env = Env()

# Append .exe on windows for native tools
GPRBUILD  = 'gprbuild' + env.host.os.exeext
GPRCONFIG = 'gprconfig' + env.host.os.exeext
GPRCLEAN  = 'gprclean' + env.host.os.exeext
XCOV      = 'xcov' + env.host.os.exeext

class BUILDER:

    COMMON_CARGS = " -g -fpreserve-control-flow "
    SCOV_CARGS = " -gnateS "

    BASE_COMMAND = GPRBUILD

    @staticmethod
    def CONFIG_COMMAND (toplev_options):

        # When --rtsgpr is provided (and non empty), e.g. for Ravenscar,
        # assume it controls the necessary --RTS flags to pass. Otherwise,
        # assume we are targetting zfp and configure to pass --RTS=zfp by
        # default for Ada

        defrts = "zfp" if not toplev_options.rtsgpr else ""

        return " ".join ((
                GPRCONFIG, '--batch',
                '--config=C --config=Asm --config=Ada,,%s' % defrts,
                '--target=%s' % env.target.triplet, '-o suite.cgpr'))



