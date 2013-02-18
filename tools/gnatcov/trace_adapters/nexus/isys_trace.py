import sys
proc_id = sys.argv [1]
executable = sys.argv [2]

import isystem.connect as ic

cmgr = ic.ConnectionMgr()
cmgr.connectMRU('.\workspace\min' + proc_id + '.xjrf')

traceDoc = ic.CTraceController (cmgr, 'min' + proc_id + '.trd', 'w')

traceDoc.select ('Everything Individual Branch')

loader   = ic.CLoaderController (cmgr)
executer = ic.CExecutionController (cmgr)

if proc_id == "5554":
  executer.resetAndRun (ic.CExecutionController.TOUT_1s)
  executer.stop ()
elif proc_id == "5634":
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
else:
  print "Unknown processor ID value: " + proc_id
  exit (1)

downloadConfig = ic.CDownloadConfiguration ()
downloadConfig.setCodeOffset (0).setSymbolsOffset (0). \
  setUsedInFullDownload (True). \
  setDownloadFileFormat (ic.CDownloadConfiguration.ftELF);

dbg = ic.CDebugFacade(cmgr)
# dbg.download()
loader.targetDownload (downloadConfig, executable,
' LoadSymbols(Global) LoadZeros(0) LoadDwarfSection(1) ReverseBitFields(0) DumpELFHeaders(0) LoadCode(Virtual) CallStack(Automatic) MergeTypes(0) GCC_ARM_double_Format(Swapped) RemoveOptimizedLines(1) InsertInlinedFunctions(0) IgnoreNonStatementLines(1) ')
isyminfo = dbg.getSymbolInfo (ic.IConnectDebug.fMonitor |
                              ic.IConnectDebug.gafExpression, '_start')
dbg.gotoAddress (0, isyminfo.getAddress () )
isyminfo = dbg.getSymbolInfo (ic.IConnectDebug.fMonitor |
                              ic.IConnectDebug.gafExpression, '_after_main')

traceDoc.start ()
executer.runUntilAddress (0, isyminfo.getAddress () )
executer.waitUntilStopped ()

formatter = ic.CTraceBinExportFormat ()
formatter.setTraceItemFlags (0)
formatter.setHeader (False) \
         .addTraceItem (ic.CTraceBinExportFormat.EOCDItem)
exportCfg = ic.CTraceExportConfig ()
exportCfg.setFileName ('nexus_trace.bin').setFormatter (formatter)
traceDoc.exportData (exportCfg)

cmgr.disconnect (ic.IConnect.dfCloseServerUnconditional | ic.IConnect.dfCloseAutoSaveNone)
