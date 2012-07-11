import sys
procid = sys.argv [1]
executable = sys.argv [2]

import isystem.connect as ic

cmgr = ic.ConnectionMgr()
cmgr.connectMRU('.\workspace\min' + procid + '.xjrf')

traceDoc = ic.CTraceController (cmgr, 'min' + procid + '.trd', 'w')

traceDoc.select ('Everything Individual Branch')

loader = ic.CLoaderController (cmgr)
executer = ic.CExecutionController (cmgr)
executer.resetAndRun (ic.CExecutionController.TOUT_1s)
executer.stop ()

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
formatter = ic.CTraceBinExportFormat ()
formatter.setTraceItemFlags (0)
formatter.setHeader (False) \
         .addTraceItem (ic.CTraceBinExportFormat.EOCDItem)
exportCfg = ic.CTraceExportConfig ()
exportCfg.setFileName ('nexus_trace.bin').setFormatter (formatter)
traceDoc.exportData (exportCfg)

cmgr.disconnect (ic.IConnect.dfCloseServerUnconditional | ic.IConnect.dfCloseAutoSaveNone)
