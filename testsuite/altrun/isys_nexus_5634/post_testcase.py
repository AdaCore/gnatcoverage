#!/usr/bin/env python


import time
import sys
import os

altrun_dir = sys.argv[1]
ws_file = altrun_dir + '\ws\j.xjrf'

import isystem.connect as ic

cmgr = ic.ConnectionMgr()
cmgr.connectMRU()
wspaceControl = ic.CWorkspaceController (cmgr)
wspaceControl.open(os.path.abspath(ws_file))

cmgr.disconnect (0)
