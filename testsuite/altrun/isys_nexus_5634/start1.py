from datetime import datetime
import sys
import os


print datetime.now().replace(microsecond=0).isoformat (' ')

ws1 = os.path.abspath(sys.argv[1])

import isystem.connect as ic

cmgr = ic.ConnectionMgr()
cmgr.connectMRU(ws1)

executer = ic.CExecutionController (cmgr)
executer.reset ()
executer.stop ()
# the steps above are included because the retrieval
# of the firmware info from the ic5000 doesn't work
# until some action has occured.

cmgr.disconnect (0)
