import time
import sys
import os
executable = sys.argv [1]

import isystem.connect as ic

cmgr = ic.ConnectionMgr()
cmgr.connectMRU()
wspaceControl = ic.CWorkspaceController (cmgr)
wspaceControl.open(os.path.abspath('.\isyswspace\justrun.xjrf'))

print 'WinIDEA workspace is now open'

traceDoc = ic.CTraceController (cmgr, 'new_trigger.trd',  'w')
triggerIdx = traceDoc.createTrigger ('new_trigger')
traceDoc.select (triggerIdx)

# Full list of available option can be found in WinIDEA Help->Display Options...
traceDoc.setTriggerOption (triggerIdx, "UseAdvancedTrigger", "TRUE")
traceDoc.setTriggerOption (triggerIdx, "EnableProfiler", "FALSE")
traceDoc.setTriggerOption (triggerIdx, "EnableCoverage", "FALSE")
#traceDoc.setTriggerOption (triggerIdx, "Trigger.Global.ProfilerOperationMode", "Range")

traceDoc.setTriggerOption (triggerIdx, "HW.Recorder.Start", "Immediately")
traceDoc.setTriggerOption (triggerIdx, "HW.Recorder.BufferSize", "1 GB")
traceDoc.setTriggerOption (triggerIdx, "HW.Recorder.TriggerPosition", "Begin")
traceDoc.setTriggerOption (triggerIdx, "HW.Recorder.StallCPU ", "FALSE")
traceDoc.setTriggerOption (triggerIdx, "HW.Recorder.BreakOnTrigger", "FALSE")

traceDoc.setTriggerOption (triggerIdx, "HW.PPC5xxx.HW.Enabled", "TRUE")
traceDoc.setTriggerOption (triggerIdx, "HW.PPC5xxx.HW.e200[0].Enabled", "TRUE")
traceDoc.setTriggerOption (triggerIdx, "HW.PPC5xxx.HW.e200[0].Anything", "FALSE")

traceDoc.setTriggerOption (triggerIdx, "HW.PPC5xxx.HW.e200[0].IAC_Enable[0]", "TRUE")
traceDoc.setTriggerOption (triggerIdx, "HW.PPC5xxx.HW.e200[0].IAC_Enable[1]", "TRUE")
traceDoc.setTriggerOption (triggerIdx, "HW.PPC5xxx.HW.e200[0].IAC_Combination[0]", "None")
traceDoc.setTriggerOption (triggerIdx, "HW.PPC5xxx.e200[0].IAC_Address[0]", "main")
traceDoc.setTriggerOption (triggerIdx, "HW.PPC5xxx.e200[0].IAC_Address[1]", "_exit")

traceDoc.setTriggerOption (triggerIdx, "HW.PPC5xxx.HW.e200[0].RecordProgram", "TRUE")
traceDoc.setTriggerOption (triggerIdx, "HW.PPC5xxx.HW.e200[0].RecordData", "FALSE")
traceDoc.setTriggerOption (triggerIdx, "HW.PPC5xxx.HW.e200[0].RecordOTM", "FALSE")
traceDoc.setTriggerOption (triggerIdx, "HW.PPC5xxx.HW.e200[0].PeriodicOTM", "FALSE")
traceDoc.setTriggerOption (triggerIdx, "HW.PPC5xxx.HW.e200[0].RecordWP", "All")
traceDoc.setTriggerOption (triggerIdx, "HW.PPC5xxx.HW.e200[0].ProgramTrace", "BranchMsg")
traceDoc.setTriggerOption (triggerIdx, "HW.PPC5xxx.HW.e200[0].PgmStart", "IAC1")
traceDoc.setTriggerOption (triggerIdx, "HW.PPC5xxx.HW.e200[0].PgmEnd", "IAC2")

loader   = ic.CLoaderController (cmgr)
executer = ic.CExecutionController (cmgr)
debug    = ic.CDebugFacade(cmgr)
status   = debug.getCPUStatus()

print 'Trying to connect to the probe/board...'

for try_nb in range(10):
    debug = ic.CDebugFacade(cmgr)
    status = debug.getCPUStatus()
    isConnected = (not status.isMustInit())
    print 'is connected: ' + str(isConnected)
    print status.toString()

    if isConnected:
        break

    try:
        debug.reset()
    except:
        print 'exception in debug.reset()'

    executer = ic.CExecutionController (cmgr)
    try:
        executer.reset()
    except:
        print 'exception in executer.reset()'
    try:
        executer.stop()
    except:
        print 'exception in executer.stop()'

    time.sleep (1)

executer.reset ()
executer.stop ()
MPCCtrl = ic.CMPC5xxxController (cmgr)
TLB = MPCCtrl.getTLB (0)
TLB.m_dwMAS0 = 0x10030000
TLB.m_dwMAS1 = 0xC0000400
TLB.m_dwMAS2 = 0x40000008
TLB.m_dwMAS3 = 0x4000003F
TLB.m_dwMAS4 = 0x00030413
TLB.m_dwMAS5 = 0x00000000
MPCCtrl.setTLB (0, TLB)

downloadConfig = ic.CDownloadConfiguration ()
downloadConfig.setCodeOffset (0).setSymbolsOffset (0). \
  setUsedInFullDownload (True). \
  setDownloadFileFormat (ic.CDownloadConfiguration.ftELF);

dbg = ic.CDebugFacade(cmgr)
loader.targetDownload (downloadConfig, executable,
' LoadSymbols(Global) LoadZeros(0) LoadDwarfSection(1) ReverseBitFields(0) DumpELFHeaders(0) LoadCode(Virtual) CallStack(Automatic) MergeTypes(0) GCC_ARM_double_Format(Swapped) RemoveOptimizedLines(1) InsertInlinedFunctions(0) IgnoreNonStatementLines(1) ')
isyminfo = dbg.getSymbolInfo (ic.IConnectDebug.fMonitor |
                              ic.IConnectDebug.gafExpression, '_start')
dbg.gotoAddress (0, isyminfo.getAddress () )
isyminfo = dbg.getSymbolInfo (ic.IConnectDebug.fMonitor |
                              ic.IConnectDebug.gafExpression, '_exit')

traceDoc.start ()

print 'Start execution...'

executer.runUntilAddress (0, isyminfo.getAddress () )
if not executer.waitUntilStopped (pollingInterval=5000, timeout=400000):
   print '!!! EXECUTION TIMEOUT !!!'

print '... execution stoped.'

last_chance = dbg.evaluate(ic.IConnectDebug.fRealTime, '__gnat_unexpected_last_chance_call')
if last_chance.getInt() != 0:
   print '!!! EXCEPTION RAISED !!!'

traceDoc.waitUntilLoaded (0, 100)
formatter = ic.CTraceBinExportFormat ()
formatter.setTraceItemFlags (0)
formatter.setHeader (False) \
         .addTraceItem (ic.CTraceBinExportFormat.EOCDItem)
exportCfg = ic.CTraceExportConfig ()
exportCfg.setFileName ('nexus_trace.bin').setFormatter (formatter)
traceDoc.exportData (exportCfg)
traceDoc.closeAll ()

cmgr.disconnect (0)
