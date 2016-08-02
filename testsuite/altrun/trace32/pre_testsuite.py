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

ofile = "cmd_.out"
p = gnatpython.ex.Run([PATH_TO_T32,
                       "-c",
                       os.path.join(altrun_dir_path, 'config.t32')
                       ],
                      output=ofile,
                      bg=True)

sleep(2)
