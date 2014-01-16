#!/usr/bin/env python

# Stop all running instances of winIDEA

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
