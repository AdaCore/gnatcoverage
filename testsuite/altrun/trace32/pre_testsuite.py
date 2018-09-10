import sys
import optparse
import os
import gnatpython.ex
from gnatpython.env import putenv
from time import sleep
from t32api import PATH_TO_T32_HOME, PATH_TO_T32

# ---------
# -- log --
# ---------
def log(str):
    print "trace32/pre_testsuite.py:" + str

altrun_dir_path = os.path.dirname(os.path.realpath(__file__))

putenv('T32SYS', PATH_TO_T32_HOME)
putenv('T32TMP', "/tmp")
putenv('T32PDFVIEWER', PATH_TO_T32_HOME + "/bin/pc_linux64/t32_startpdfviewer.sh")

# Run t32usbchecker to reset the probe in case it is already connected.
#
# This can happen when the testsuite crashes, for instance with an rlimit. The
# probe is still trying to talk with the previous Trace32 instance that was
# killed by rlimit.
#
# It should be possble to achive the same result with the
# CONNECTIONMODE=AUTOCONNECT parameter of the configuration, however this
# parameter doesn't work with the SCREEN=OFF mode.
p = gnatpython.ex.Run([os.path.dirname(PATH_TO_T32) + '/t32usbchecker'],
                      output="t32usbchecker.out",
                      bg=False)

p = gnatpython.ex.Run([PATH_TO_T32,
                       "-c",
                       os.path.join(altrun_dir_path, 'config.t32')
                       ],
                      output="t32.out",
                      bg=True)

sleep(2)
