from datetime import datetime
import sys
import os


print datetime.now().replace(microsecond=0).isoformat (' ')

ws1 = os.path.abspath(sys.argv[1])

import isystem.connect as ic

cmgr = ic.ConnectionMgr()
cmgr.connectMRU(ws1)

cmgr.disconnect (0)
