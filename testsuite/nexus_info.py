import gnatpython.fileutils as fu
import isystem.connect as ic

cmgr = ic.ConnectionMgr()
cmgr.connectMRU('')
ideController = ic.CIDEController(cmgr)

def winIDEA_version():
    return ideController.getWinIDEAVersion().toString()

def firmware_revision():
    firmwareDateInstalledUrl = '/iOPEN/SystemConfig.Firmware.DateInstalled'
    firmwareDateInstalled = ideController.getOptionInt(firmwareDateInstalledUrl)
    year = firmwareDateInstalled >> 16
    month = (firmwareDateInstalled & 0xFF00) >> 8
    date = (firmwareDateInstalled & 0xFF)

    return '{}{:02d}{:02d}'.format(year, month, date)

def probe_model():
    # No way to retrieve this automatically. This is what we have:
    return 'iSystem iC5000'

def workspace_file():
    return ' '.join(fu.find (root="altrun", pattern="justrun.xjrf"))

print (
    "Nexus Framework ## "
    "Probe Model: %s, "
    "Firmware: %s, "
    "winIDEA: %s, "
    "Workspace: %s") % \
    (probe_model(), firmware_revision(), winIDEA_version(), workspace_file())

cmgr.disconnect(0)
