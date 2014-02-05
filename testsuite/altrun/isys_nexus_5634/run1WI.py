#!/usr/bin/env python

# Stop all running instances of winIDEA and then
# start a single instance using the simple workspace
# in the 'ws' directory.

import os
import isystem.connect as ic

cmgr = ic.ConnectionMgr ()

connectionConfig = ic.CConnectionConfig ()

port = 0
while (1):
  port = cmgr.findExistingInstance ('', connectionConfig)
  if (port < 0):
    break;
  else:
    print 'stopping winIDEA at port: ', port
    cmgr.connect('', port)
    cmgr.disconnect (ic.IConnect.dfCloseServerUnconditional | ic.IConnect.dfCloseAutoSaveNone)


ws1 = os.path.abspath('ws/j.xjrf')

#cmgr = ic.ConnectionMgr()

#connectionConfig = ic.CConnectionConfig()
connectionConfig.workspace (ws1)
port = cmgr.startNewInstance (connectionConfig)
print 'new winIDEA at port: ', port

cmgr.connect(connectionConfig)
executer = ic.CExecutionController (cmgr)
executer.reset ()
executer.stop ()
# the steps above are included because the retrieval
# of the firmware info from the ic5000 doesn't work
# until some action has occured.
