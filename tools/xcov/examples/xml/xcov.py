#!/usr/bin/env python
"""API to read xcov XML coverage reports"""

from xml.dom import minidom
from time import strftime


COVERAGE_STATE = {
    '.': 'no_code', # no code for this line,
    '+': 'covered', # the coverage criteria is satisfied
    '-': 'not_covered', # no instructions executed
    '!': 'partially_covered'} # the coverage criteria is partially satisfied on
                              # this line

LEVELS = ('branch', 'insn', 'stmt', 'decision', 'mcdc')

def is_covered(obj):
    """Return True if obj is marked as covered or no_code"""
    return obj.coverage in ('no_code', 'covered')


class Trace(object):
    """A trace info file

    it represents a trace files given to the coverage tool
    """

    def __init__(self, filename, program, date, tag):
        """Create a new trace info file object

        filename : name of the trace file on the host file system.
        program  : name of the executable program on the host file system.
        date     : date of the run that generated the trace file.
        tag      : trace file tag.
        """
        self.filename = filename
        self.program = program
        self.date = date
        self.tag = tag

    def __str__(self):
        """Pretty print the object content"""
        return """ %(filename)s
  program: %(program)s
  date: %(date)s
  tag: %(tag)s""" % self.__dict__


class SrcLine(object):
    """Represents a line of source code."""

    def __init__(self, xml_node=None):
        """Create a new line object

        Attributes:
        num : NUM; line number in source code.
        src : TEXT; copy of the line as it appears in the source code.
        """
        self.num = 0
        self.src = ""
        self.exempted = False
        self.column_begin = 0

        if xml_node is not None:
            self.parse_node(xml_node)

    def parse_node(self, xml_node):
        """Parse a <line /> xml node (inside a src_mapping/src)"""
        self.num = int(xml_node.getAttribute('num'))
        self.src = xml_node.getAttribute('src')
        self.exempted = bool(xml_node.getAttribute('exempted'))


class RefLine(SrcLine):
    """Represents a line of source code."""

    def __init__(self, xml_node=None):
        """Reference a line of code

        Attributes:
        num : line number in source code.
        src : copy of the line as it appears in the source code.
        column_begin : column number for the beginning of the coverage
                       item we are considering.
        column_end   : column number for the end of the coverage item we are
                       considering
        exempted : BOOLEAN; True if the current line is covered by an exemption
        """
        self.coverage = None
        self.column_end = 0
        SrcLine.__init__(self, xml_node)

    def parse_node(self, xml_node):
        SrcLine.parse_node(self, xml_node)
        if xml_node.hasAttribute('coverage'):
            self.coverage = COVERAGE_STATE[xml_node.getAttribute('coverage')]
        if xml_node.hasAttribute('column_begin'):
            self.column_begin = int(xml_node.getAttribute('column_begin'))
        if xml_node.hasAttribute('column_end'):
            self.column_end = int(xml_node.getAttribute('column_end'))


class Statement(object):

    def __init__(self, sco_id, text, coverage):
        self.sco_id = int(sco_id)
        self.text = text
        self.coverage = COVERAGE_STATE[coverage]
        self.src_lines = []


class Decision(object):

    def __init__(self, xml_node=None):
        self.sco_id = 0
        self.text = ""
        self.coverage = None
        self.src_lines = []
        self.conditions = [] # Associated mcdc conditions

        if xml_node is not None:
            self.parse_node(xml_node)

    def parse_node(self, node):
        self.sco_id = int(node.getAttribute('id'))
        self.text = node.getAttribute('text')
        self.coverage = COVERAGE_STATE[node.getAttribute('coverage')]
        # Get only direct src child (condition inside decision contain src
        # node)
        top_src = [n for n in node.childNodes
                   if hasattr(n, 'tagName') and n.tagName == 'src']
        assert(len(top_src) == 1)
        self.src_lines = [RefLine(xml_node=n)
                          for n in top_src[0].getElementsByTagName('line')]
        for cond in node.getElementsByTagName('condition'):
            self.conditions.append(Condition(cond))


class Condition(object):
    """Represent a condition"""

    def __init__(self, xml_node=None):
        self.sco_id = 0
        self.text = ""
        self.coverage = None
        self.src_lines = []

        if xml_node is not None:
            self.parse_node(xml_node)

    def parse_node(self, node):
        self.sco_id = int(node.getAttribute('id'))
        self.text = node.getAttribute('text')
        self.coverage = node.getAttribute('coverage')
        for src in node.getElementsByTagName('src'):
            self.src_lines = parse_src_node(src, None)


class Message(object):

    def __init__(self, kind, sco, message):
        self.kind = kind
        self.sco = sco
        self.message = message
        self.sco_id = 0


def parse_src_node(node, parent):
    return [RefLine(xml_node=n) for n in node.getElementsByTagName('line')]


def parse_message_node(node):
    m = Message(
        kind=node.getAttribute('node'),
        sco=node.getAttribute('SCO'),
        message=node.getAttribute('message'))
    if m.sco:
        m.sco_id = int(m.sco.split(':')[0].split('#')[1])
    return m


def parse_statement_node(node, parent):
    stmt = Statement(
        sco_id=int(node.getAttribute('id')),
        text=node.getAttribute('text'),
        coverage=node.getAttribute('coverage'))
    for src in node.getElementsByTagName('src'):
        stmt.src_lines = parse_src_node(src, parent)
    return stmt


class InstructionSet(object):
    """Represent a set of instruction"""

    def __init__(self, xml_node=None, srclines=None):
        self.coverage = None

        if xml_node is not None:
            self.parse_node(xml_node)

        if srclines is not None:
            self.src_lines = srclines

    def parse_node(self, node):
        self.coverage = COVERAGE_STATE[node.getAttribute('coverage')]


class SrcMapping(object):

    def __init__(self, source, xml_node=None):
        self.source = source
        self.coverage = None
        self.statement = []
        self.decision = []
        self.messages = []

        if xml_node is not None:
            self.parse_node(xml_node)

    def parse_node(self, node):
        # Get only direct src child
        top_src = [n for n in node.childNodes
                   if hasattr(n, 'tagName') and n.tagName == 'src']
        assert(len(top_src) == 1)
        self.src_lines = [SrcLine(xml_node=n)
                          for n in top_src[0].getElementsByTagName('line')]

        self.coverage = COVERAGE_STATE[node.getAttribute('coverage')]
        self.statement = [parse_statement_node(n, self) for n in
                           node.getElementsByTagName('statement')]
        self.decision = [Decision(n) for n in
                          node.getElementsByTagName('decision')]
        self.messages = [parse_message_node(n) for n in
                         node.getElementsByTagName('message')]
        self.instruction_set = [InstructionSet(n, self.src_lines) for n in
                                node.getElementsByTagName('instruction_set')]

        # Register messages
        for m in self.messages:
            self.source.messages[m.sco_id] = m
        for l in self.src_lines:
            self.source.lines[l.num] = l

    def get_non_exempted_violations(self, levels):
        """Report violations (as done by xcov --annotate=report)

        When two coverage criteria are not met on the same line, only
        report errors for the "lowest" one. For example, if a decision is
        not covered for stmt coverage, it will certainly not be covered
        for decision coverage or MCDC; but report only the stmt coverage
        error.
        """

        def get_msg(obj, level):
            """Get Error message

            If error/warning messages have been attached to the line, they
            will be printed in the report; otherwise, fall back to a
            general error message.
            """
            if hasattr(obj, 'sco_id') and obj.sco_id in self.source.messages:
                return "%s: %s" % (obj.__class__.__name__.upper(),
                                   self.source.messages[obj.sco_id].message)
            else:
                covstatus = obj.coverage
                if obj.__class__.__name__.lower() == 'instructionset':
                    return "line %s for %s coverage" % (covstatus.upper(),
                                                        level.upper())
                elif obj.__class__.__name__.lower() == 'decision' and level == 'mcdc':
                    for cond in obj.conditions:
                        if not is_covered(cond):
                            return get_msg(cond, level)
                else:
                    return "%s not covered" % obj.__class__.__name__.lower()

        level_to_obj_name = {'stmt': 'statement',
                             'mcdc': 'decision',
                             'branch': 'instruction_set',
                             'insn' : 'instruction_set'}

        for level in LEVELS:
            msg = []
            if level in levels:
                for obj in self.__dict__[level_to_obj_name.get(level, level)]:
                    if not is_covered(obj):
                        msg.append((get_msg(obj, level), obj))
            if msg:
                return msg

class SourceFile(object):

    def __init__(self, filename):
        self.__xml = minidom.parse(filename)
        # this document should contain only one <source> node
        self.__source = self.__xml.getElementsByTagName('source')[0]
        self.filename = self.__source.getAttribute('file')
        self.coverage_level = self.__source.getAttribute('coverage_level')
        self.messages = {} # message id -> Message
        self.lines = {} # line number -> Line

        # Parse all src_mapping
        self.src_mappings = [
            SrcMapping(source=self, xml_node=n) for n in
            self.__source.getElementsByTagName('src_mapping')]

    def get_lines(self):
        return [line for sm in self.src_mappings
                for line in sm.src_lines]

    def get_non_exempted_violations(self, levels):
        list_ = [sm.get_non_exempted_violations(levels) for sm in self.src_mappings]
        list_ = [l for l in list_ if l is not None]
        result = []
        for l in list_:
            result.extend(l)
        return result

    def pp_violations(self, levels):
        violations_list = self.get_non_exempted_violations(levels)
        violations_list.sort(key=lambda x: x[1].sco_id if hasattr(x[1], 'sco_id') else 0)

        return "\n".join(["%s:%d:%d: %s" % (
                self.filename, line.num,
                line.column_begin, msg)
                for msg, obj in violations_list
                for line in obj.src_lines])


class XCovReport(object):

    def __init__(self, report_name='index.iml'):
        self.__xml = minidom.parse(report_name)
        self.coverage_level = None
        self.trace_file = []
        self.source_names = []

        # Get trace files
        for element in self.__xml.getElementsByTagName('coverage_report'):
            self.coverage_level = element.getAttribute('coverage_level')
            for cov_info in element.getElementsByTagName('coverage_info'):
                for include in cov_info.getElementsByTagName('xi:include'):
                    # Open included file to get all trace object
                    for trace in minidom.parse(
                        include.getAttribute(
                            'href')).getElementsByTagName('trace'):
                        self.trace_file.append(Trace(
                            filename=trace.getAttribute('filename'),
                            program=trace.getAttribute('program'),
                            date=trace.getAttribute('date'),
                            tag=trace.getAttribute('tag')))
        self.source_names = [
            include_file.getAttribute('href')
            for sources in self.__xml.getElementsByTagName('sources')
            for include_file in sources.getElementsByTagName('xi:include')]

    def get_levels(self):
        return [p for p in self.coverage_level.split('+')]

    def visitor(self, func):
        for src in self.source_names:
            func(SourceFile(src))

    def get_lines(self):
        return [line for s in self.sources for line in s.get_lines()]
