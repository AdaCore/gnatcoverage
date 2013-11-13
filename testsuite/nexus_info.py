import isystem.connect as ic

cmgr = ic.ConnectionMgr()
cmgr.connectMRU()
ide = ic.CIDEController (cmgr)
print "Nexus Framework ## winIDEA: ide.getWinIDEAVersion().toString()"
cmgr.disconnect(0)
