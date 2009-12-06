#!/usr/bin/env python

# ****************************************************************
# ** Very basic example of Xcov XML output processing in python **
# ****************************************************************

from xml.dom import minidom

class XcovParser:
    pass

class XcovIndexParser (XcovParser):

    def __init__(self, unit):
        self.doc = minidom.parse('index.xml')

class Xdecision:
    def __init__(self, node):
        self.srcNode = node.getElementsByTagName("src")[0]
        self.lineNode = self.srcNode.getElementsByTagName("line")[0]

class XcovUnitParser (XcovParser):

    def __init__(self, unit):
        self.unit = unit
        self.doc = minidom.parse(unit+'.xml')

    def decisionNodes(self):
        return self.doc.getElementsByTagName("decision")
    
    def processDecisionNode(self, node):
        xd = Xdecision(node)
        print "%s : %s" % (node.getAttribute("coverage"), 
                           xd.lineNode.getAttribute("src"))

    def scanDecisions(self):
        dnodes = self.decisionNodes()
        [self.processDecisionNode(dn) for dn in dnodes]
        
p = XcovUnitParser ("services.adb")
p.scanDecisions()
