import isystem.connect as ic

cmgr = ic.ConnectionMgr()
cmgr.connectMRU('')

ideController = ic.CIDEController(cmgr)
winIDEA_ver=ideController.getWinIDEAVersion().toString()

modulesUrl = '/iOPEN/BBInfo.Modules'
firmwareDateInstalledUrl = '/iOPEN/SystemConfig.Firmware.DateInstalled'
firmwareDateInstalled = ideController.getOptionInt(firmwareDateInstalledUrl)
year = firmwareDateInstalled >> 16
month = (firmwareDateInstalled & 0xFF00) >> 8
date = (firmwareDateInstalled & 0xFF)
firmware_rev='{}{:02d}{:02d}'.format(year, month, date)

print "Nexus Framework ## winIDEA: %s, Firmware: %s" % \
    (winIDEA_ver, firmware_rev)

cmgr.disconnect(0)
