import time
import sys
executable = sys.argv [1]

import isystem.connect as ic

cmgr = ic.ConnectionMgr()
cmgr.connectMRU('.\isyswspace\justrun.xjrf')

traceDoc = ic.CTraceController (cmgr, 'justrun.trd', 'w')

traceDoc.select ('justrun')

loader   = ic.CLoaderController (cmgr)
executer = ic.CExecutionController (cmgr)

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

isyminfo = dbg.getSymbolInfo (ic.IConnectDebug.fMonitor |
                              ic.IConnectDebug.gafExpression, '__gnat_unexpected_last_chance_call')
varType = ic.SType()
varType.m_byBitSize = 32
varType.m_byType = ic.SType.tSigned
last_chance = dbg.readValue (ic.IConnectDebug.fMonitor, isyminfo.getMemArea (), isyminfo.getAddress (), varType)
#print 'LAST CHANCE VALUE'
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
time.sleep (6.0)

cmgr.disconnect (ic.IConnect.dfCloseServerUnconditional | ic.IConnect.dfCloseAutoSaveNone)
