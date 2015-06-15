#!/usr/bin/env python

# Stop all running instances of winIDEA and then
# start a single instance using the simple workspace
# in the 'ws' directory.

import os
import isystem.connect as ic
import time

cmgr = ic.ConnectionMgr ()

connectionMgr = ic.ConnectionMgr()
connectionConfig = ic.CConnectionConfig()
winIDEAInstances = ic.VectorWinIDEAInstanceInfo()

hostAddress = ''  # enumerate instances on local host. You may also specify remote host
                  # here, for example as IP address: '10.1.2.91'
connectionMgr.enumerateWinIDEAInstances(hostAddress, connectionConfig, winIDEAInstances)

# Now we'll connect to each of found winIDEA instances and close them
for instance in winIDEAInstances:
    instanceCMgr = ic.ConnectionMgr()
    instanceCMgr.connect(hostAddress, instance.getTcpPort())
    instanceCMgr.disconnect (ic.IConnect.dfCloseServerUnconditional | ic.IConnect.dfCloseAutoSaveNone)
    print 'Kill WinIDEA at port: ', instance.getTcpPort()

ws1 = os.path.abspath('ws/j.xjrf')
cmgr = ic.ConnectionMgr()
connectionConfig = ic.CConnectionConfig()
connectionConfig.visibility(ic.IConnect.lfShowHidden)

print "Openning workspace at '%s'" % ws1
connectionConfig.workspace (ws1)

port = cmgr.startNewInstance (connectionConfig)
print 'new winIDEA at port: ', port

cMgr = ic.ConnectionMgr()
cMgr.connectMRU('')

ide = ic.CIDEController(cMgr)
print 'winIDEA version : ' + ide.getWinIDEAVersion().toString()

# Set probe communication options
ide.setOption('/IOPEN/Communication.USBDeviceName', 'iC5000 (SN 68020)')
ide.setOption('/IOPEN/Communication.IPAddress', 'iC5000 (SN 68020) : 5313')
ide.setOption('/IOPEN/Communication.Mode', 'USB')
ide.commitOptions('/IOPEN/')

for try_nb in range(10):
    debug = ic.CDebugFacade(cMgr)
    status = debug.getCPUStatus()
    isConnected = (not status.isMustInit())
    print 'is connected: ' + str(isConnected)
    if isConnected:
        break

    executer = ic.CExecutionController (cMgr)
    try:
        executer.reset()
    except:
        print 'exception in executer.reset()'
    try:
        executer.stop()
    except:
        print 'exception in executer.reset()'

    time.sleep (1)

executer = ic.CExecutionController (cMgr)
executer.reset ()
executer.stop ()
# the steps above are included because the retrieval
# of the firmware info from the ic5000 doesn't work
# until some action has occured.
