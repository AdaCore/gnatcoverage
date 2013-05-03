#! /usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import collections
import os
import re
import struct
import subprocess

import bddinfo
import slocinfo
import traceinfo

COLOR_JUMP                  = '"#000000"'
COLOR_BRANCH_FALLTHROUGH    = '"#c00000"'
COLOR_BRANCH_TAKEN          = '"#00c000"'
COLOR_OUTSIDE_JUMP          = '"#a0a0a0"'
COLOR_COVERED               = '"#008000"'
COLOR_NOT_COVERED           = '"#a00000"'

SLOC_RANGE = re.compile(
    '^([^:]*):(\d+):(\d+)-(\d+):(\d+)$'
)
SlocRange = collections.namedtuple('SlocRange',
    'filename'
    ' start_line start_column'
    ' end_line end_column'
)
Program = collections.namedtuple('Program', 'filename arch')
# A program is the `filename` ELF file, and when disassembled, it can contain
# the inconditional `jumps` instructions and the `branches` instructions.

# Decoder for the e_machine ELF header field. Depend on the EI_DATA header
# field.
ARCH_STRUCT = {
    1: struct.Struct('<H'),
    2: struct.Struct('>H'),
}

class Arch(object):
    JUMP = 'jump'
    BRANCH = 'branch'

    # To be filled by the cross-compile toolchain prefix (if any).
    PREFIX = None

    @classmethod
    def get_tool(cls, name):
        if cls.PREFIX:
            return '{}-{}'.format(cls.PREFIX, name)
        else:
            return name

    @staticmethod
    def get_insn_properties(insn):
        """Return (Arch.JUMP, <jump addr>) for unconditionnal jump
        instructions, (Arch.BRANCH, <branch addr>) for conditionnal branch
        instructions and (None, None) for all other instructions. When the
        destination address in unknown, None can be returned instead.
        """
        raise NotImplementedError()

class ArchX86(Arch):
    JUMPS = set('jmp jmpl'.split())
    BRANCHES = set(
        'ja jae jb jbe jc jcxz jecxz je jg jge jl jle jna jnae jnb jnbe jnc'
        ' jne jng jnge jnl jnle jno jnp jns jnz jo jp jpe jpo js jz'.split()
    )

    @staticmethod
    def get_insn_dest(insn):
        return int(insn.operands.split()[0], 16)

    @staticmethod
    def get_insn_properties(insn):
        if insn.mnemonic in ArchX86.JUMPS:
            return (Arch.JUMP, ArchX86.get_insn_dest(insn))
        elif insn.mnemonic in ArchX86.BRANCHES:
            return (Arch.BRANCH, ArchX86.get_insn_dest(insn))
        else:
            return (None, None)

class ArchPPC32(Arch):
    PREFIX = 'powerpc-elf'

    # Mnemonic tables: mnemonic -> index of operand that contain the address,
    # or None (for instance, for a register).
    JUMPS = {
        'b': None,
        'ba': 0,
    }
    # bl and blr return from subroutine: not jump/branch instructions.
    @staticmethod
    def get_insn_dest(operand):
        return int(operand.split()[0], 16)

    @staticmethod
    def get_insn_properties(insn):
        if not insn.mnemonic.startswith('b'):
            return (None, None)

        # Strip prediction any hint.
        mnemonic = insn.mnemonic.rstrip('+-')

        # Branch can go to an absolute address, to the link register or to the
        # control register.
        dest_in_reg = False
        return_from_subroutine = False
        if mnemonic.endswith('l') or mnemonic.endswith('la'):
            # Branch and Link (call)
            return (None, None)
        elif mnemonic.endswith('lr'):
            # To Link Register (return)
            return (None, None)
        elif mnemonic.endswith('ctr'):
            # To ConTrol Register (destination known at runtime)
            return (Arch.BRANCH, None)
        elif mnemonic.endswith('a'):
            mnemonic = mnemonic[:-1]

        operands = insn.operands.split(',')
        if ' ' in operands[0]:
            dest = ArchPPC32.get_insn_dest(operands[0])
        else:
            dest = ArchPPC32.get_insn_dest(operands[1])

        return (
            Arch.BRANCH if mnemonic != 'b' else Arch.JUMP,
            dest
        )

ARCHITECTURES = {
    # x86
    3:  ArchX86,
    # PowerPC 32bit
    20: ArchPPC32,
    # x86_64
    62: ArchX86,
}

OBJDUMP_INSN = re.compile(
    '^[ ]*(?P<pc>[0-9a-f]+):'
    '\t[0-9a-f ]+\t'
    '(?P<mnemonic>[^ ]+)'
    '(?:[ ]+(?P<operands>.+))?\n$'
)

def parse_program(string):
    """Check that `string` is a valid ELF and get the architecture from it.

    Return (string, architecture).
    """
    with argparse.FileType('rb')(string) as f:
        # Read the ELF header just to get the machine type.
        elf_ident = f.read(16)
        if elf_ident[:4] != b'\x7fELF':
            raise argparse.ArgumentTypeError('{}: not an ELF'.format(string))
        ei_data = ord(elf_ident[5:6])
        arch_struct = ARCH_STRUCT[ei_data]

        # Pass the e_type ELF header field.
        _ = f.read(2)

        # Here is the e_machine field!
        elf_machine,  = arch_struct.unpack(f.read(2))
        try:
            arch = ARCHITECTURES[elf_machine]
        except:
            raise argparse.ArgumentTypeError(
                '{}: unhandled architecture ({})'.format(
                    string, elf_machine
                )
            )
        else:
            return Program(string, arch)

def parse_sloc_range(string):
    m = SLOC_RANGE.match(string)
    if not m:
        raise argparse.ArgumentTypeError(
            'Invalid sloc range: {}'.format(string)
        )
    filename, start_line, start_column, end_line, end_column = m.groups()
    return SlocRange(
        filename.encode('ascii'),
        int(start_line), int(start_column),
        int(end_line), int(end_column),
    )


def slocs_match_range(slocs, sloc_range):
    """Return if any of the `slocs` match `sloc_range`."""
    for sloc in slocs:
        if (
            sloc_range.filename not in sloc.filename or
            sloc.line < sloc_range.start_line or
            sloc.line > sloc_range.end_line
        ):
            continue
        elif sloc.line == sloc_range.start_line:
            if sloc.column is None or sloc.column >= sloc_range.start_column:
                return True
        elif sloc.line == sloc_range.end_line:
            if sloc.column is None or sloc.column <= sloc_range.end_column:
                return True
        else:
            return True

    # If there is no sloc, the associated instruction cannot be in the
    # decision.
    return False


class Insn:
    """A single instruction. It knows if it ends a basic block and which are
    its execution successors."""
    def __init__(self, pc, mnemonic, operands, slocs=None):
        self.pc = pc
        self.mnemonic = mnemonic
        self.operands = operands
        self.slocs = slocs
        self.successors = []
        self.ends_basic_block = False

    def add_successor(self, pc, end_basic_block=False, first=False):
        if first:
            self.successors.insert(0, pc)
        else:
            self.successors.append(pc)
        if end_basic_block:
            self.end_basic_block()

    def end_basic_block(self):
        self.ends_basic_block = True

    def __repr__(self):
        return 'Insn({:x} {} {})'.format(
            self.pc, self.mnemonic, self.operands
        )

# An instruction that is the successor or that have as a successor an
# instruction that is inside the wanted decision.
OutsideInsn = collections.namedtuple('OutsideInsn', 'pc successors slocs')


def get_decision_cfg(program, sloc_info, decision_sloc_range):
    get_insn_properties = program.arch.get_insn_properties

    # Let objdump disassemble the program for us...
    args = [program.arch.get_tool('objdump'), '-d', program.filename]
    print('Disassembling: {}'.format(args))
    proc = subprocess.Popen(
        args,
        stdin=open(os.devnull, 'rb'), stdout=subprocess.PIPE
    )

    # ... and filter instructions in the given sloc range.
    # This list will contain the list of instructions we are interested in.
    instructions = []
    outside_instructions = {}
    # And this will contain addresses of instructions that must start a basic
    # block.
    basic_block_starters = set()
    # True if and only if the last visited instruction can fallthrough the
    # current one.
    last_instruction_can_fallthrough = False
    # True if and only if the last visited instruction was inside the decision.
    last_instruction_in_decision = False
    last_instruction = None

    def add_outside(insn, successor):
        try:
            outside = outside_instructions[insn.pc]
        except KeyError:
            outside = OutsideInsn(insn.pc, [], insn.slocs)
            outside_instructions[insn.pc] = outside
        outside.successors.append(successor)

    while True:
        # Read as many lines as possible from objdump
        line = proc.stdout.readline().decode('ascii')
        if not line:
            break

        # Process instructions only
        m = OBJDUMP_INSN.match(line)
        if not m:
            continue

        pc = int(m.group('pc'), 16)
        insn = Insn(
            pc, m.group('mnemonic'), m.group('operands'),
            sloc_info.get(pc, [])
        )
        sloc_in_decision = slocs_match_range(insn.slocs, decision_sloc_range)

        # This instruction is the successor of the previous instruction.
        if last_instruction_can_fallthrough:
            if last_instruction_in_decision:
                instructions[-1].add_successor(insn.pc, first=True)
            elif sloc_in_decision:
                # Here, the previous instruction was not in the decision, but
                # we are interested in it anyway.
                add_outside(last_instruction, insn.pc)

        # Add this instruction if it belongs to the decision.
        if sloc_in_decision:
            instructions.append(insn)
        elif last_instruction_can_fallthrough and last_instruction_in_decision:
            # Out of the decision: end the previous basic block if needed.
            instructions[-1].end_basic_block()

        # If this is a jump/branch, it ends its own basic block and it must
        # break some other basic block.
        insn_type, dest = get_insn_properties(insn)
        if insn_type is not None:
            insn.add_successor(dest, end_basic_block=True)

            if dest is not None:
                dest_slocs = sloc_info.get(dest, [])

                # Remember the jump/branch destination must start a basic block
                # only in the case it is inside the decision.
                if slocs_match_range(dest_slocs, decision_sloc_range):
                    basic_block_starters.add(dest)
                    # If the current instruction is outside, remember it
                    # anyway.
                    if not sloc_in_decision:
                        add_outside(insn, dest)

        # Update "last_*" information for the next iteration.
        last_instruction_can_fallthrough = insn_type is not Arch.JUMP
        last_instruction_in_decision = sloc_in_decision
        last_instruction = insn

    # Break basic blocks for instructions that must start one.
    for insn in instructions:
        if any(
            successor in basic_block_starters
            for successor in insn.successors
        ):
            insn.end_basic_block()

    # Convert the instructions list to a graph data structure.
    cfg = {}
    current_bb_pc = None
    current_bb = []
    for insn in instructions:
        if not current_bb:
            current_bb_pc = insn.pc
        current_bb.append(insn)
        if insn.ends_basic_block:
            cfg[current_bb_pc] = current_bb
            current_bb = []
    if current_bb:
        cfg[current_bb_pc] = current_bb
    # Now, dangling edges are jumps/branches destinations that are out of the
    # decision.
    return cfg, outside_instructions


if __name__ == '__main__':
    import sys

    parser = argparse.ArgumentParser(
        description='Build the CFG for some decision in a program'
    )
    parser.add_argument(
        '-o', '--output', type=argparse.FileType('w'), default=sys.stdout,
        dest='output',
        help='File to output the dot graph to (default: stdout)'
    )
    parser.add_argument(
        '-b', '--basename', action='store_true',
        help='Only print basename in source locations'
    )
    parser.add_argument(
        '-B', '--bdd', dest='scos',
        help='Use SCOS to display the binary decision diagram (BDD)'
    )
    parser.add_argument(
        '-t', '--traces', dest='traces',
        help='Use a set of traces to hilight executed instructions'
    )
    parser.add_argument(
        'program', type=parse_program,
        help='The program to analyse'
    )
    parser.add_argument(
        'sloc-range', type=parse_sloc_range,
        help=(
            'Source location range of the decision to analyse'
            ' (example: 10:5-11:21)'
        )
    )

    args = parser.parse_args()

    sloc_info = slocinfo.get_sloc_info(args.program.filename)
    decision_cfg, outside_insns = get_decision_cfg(
        args.program,
        sloc_info, vars(args)['sloc-range']
    )

    # Load the BDD if asked to. Reminder: this is a map:
    #   branch instruction adresss -> branch info (associated condition and
    #   edges info).
    bdd_info = (
        bddinfo.get_bdd_info(args.program.filename, args.scos)
        if args.scos is not None else
        {}
    )

    # Load traces if asked to.
    trace_info = (
        traceinfo.get_trace_info(args.traces)
        if args.traces is not None else
        None
    )

    # Use the BDD, and especially its EXCEPTION edges info to remove successors
    # of basic blocks that raise an exception.
    for pc, basic_block in decision_cfg.items():
        last_insn = basic_block[-1]
        try:
            branch_info = bdd_info[last_insn.pc]
        except KeyError:
            continue

        def check_edge(edge_info, dest_pc):
            if edge_info is None:
                # gnatcov could not get any information about this branch:
                # nothing to do.
                return

            if isinstance(edge_info.dest_kind, bddinfo.DestRaiseException):
                try:
                    next_basic_block = decision_cfg[dest_pc]
                except KeyError:
                    pass
                else:
                    next_basic_block[-1].successors = []

        check_edge(branch_info.edge_fallthrough, last_insn.successors[0])
        check_edge(branch_info.edge_branch, last_insn.successors[1])

    # Mapping: condition SCO number -> list of dot nodes. Used to sort basic
    # blocks per decision. Basic blocks that are not associated to any
    # condition are filed under the None list. When the BDD is not loaded,
    # everything goes to the None list (this is automatic, since the BDD is
    # empty, then).
    by_condition = collections.defaultdict(list)
    condition_sloc_ranges = {}
    # List of dot edges.
    edges = []

    # At the end, (destinations - nodes) will be the set of jump/branches
    # destinations that are out of the decision.
    destinations = set()
    nodes = set()

    def pc_to_name(pc, unknown=False):
        if unknown:
            return 'bb_{:x}_unknown_dest'.format(pc)
        else:
            return 'bb_{:x}'.format(pc)

    def get_bb_condition(basic_block):
        """Return the condition the basic block belongs to or None if there is
        no such condition. Return also the branch info corresponding to the
        ending branch instruction, if any.
        """
        visited_pc = {basic_block[0].pc}

        def helper(basic_block):
            last_insn = basic_block[-1]
            try:
                branch_info = bdd_info[last_insn.pc]
            except KeyError:
                # Return the condition of the next basic block, if there is
                # only one successor.
                if len(last_insn.successors) == 1:
                    # But do not recurse endlessly.
                    next_pc = last_insn.successors[0]
                    if next_pc not in visited_pc:
                        visited_pc.add(next_pc)
                        try:
                            next_basic_block = decision_cfg[next_pc]
                        except KeyError:
                            pass
                        else:
                            return helper(next_basic_block)[0], None
                # By default, return that we got nothing.
                return None, None
            else:
                return (branch_info.cond_sco_no, branch_info)

        return helper(basic_block)

    def format_edge_info(edge_info, condition):
        if edge_info is None:
            return ['???']

        result = []

        if edge_info.cond_eval is not None:
            result.append('Cond #{} is {}'.format(
                condition, edge_info.cond_eval
            ))

        dest_kind = edge_info.dest_kind
        if isinstance(dest_kind, bddinfo.DestOutcome):
            result.append('Outcome {}'.format(
                '???' if dest_kind.value is None else dest_kind.value
            ))
        elif isinstance(dest_kind, bddinfo.DestRaiseException):
            result.append('EXCEPTION')
        elif isinstance(dest_kind, bddinfo.DestUnknown):
            result.append('???')

        return result or None

    def format_text_label(lines):
        label = ''.join('{}\n'.format(line) for line in lines)
        label = label.replace('\\', '\\\\').replace('"', '\\"')
        label = label.replace('\n', '\\l')
        return '"{}"'.format(label)

    def html_escape(line):
        for char, escape in (
            ('&', 'amp'),
            ('<', 'lt'), ('>', 'gt')
        ):
            line = line.replace(char, '&{};'.format(escape))
        return line

    def html_color(line, color=None):
        if color:
            return '<FONT COLOR={}>{}</FONT>'.format(
                color, html_escape(line)
            )
        else:
            return html_escape(line)

    def format_html_label(lines):
        label = ''.join('{}<BR ALIGN="left"/>'.format(line) for line in lines)
        return '<{}>'.format(label)

    def add_edge(from_, to, label, color):
        # Handle nicely unknown branch destinations.
        if to is None:
            to_name = pc_to_name(from_, unknown=True)
            add_node(
                pc, None,
                format_text_label(['???']),
                shape='ellipse', unknown=True
            )
        else:
            to_name = pc_to_name(to)
            destinations.add(to)
        edges.append('    {} -> {} [{}color={}];'.format(
            pc_to_name(from_), to_name,
            'label={}, '.format(format_text_label(label)) if label else '',
            color
        ))

    def add_node(pc, condition, label, shape='box', unknown=False):
        by_condition[condition].append(
            '    {} [shape={}, fontname=monospace, label={}];\n'.format(
                pc_to_name(pc, unknown), shape, label
            )
        )
        nodes.add(pc)

    for pc, basic_block in decision_cfg.items():
        # Draw the box for the basic block.
        label = []
        last_slocs = []
        for insn in basic_block:
            if insn.slocs != last_slocs:
                for sloc in insn.slocs:
                    label.append(slocinfo.format_sloc(sloc, args.basename))
                last_slocs = insn.slocs
            if trace_info:
                color = (
                    COLOR_COVERED
                    if insn.pc in trace_info else
                    COLOR_NOT_COVERED
                )
            else:
                color = None
            label.append(html_color('  {:x} {:<8}{}'.format(
                insn.pc, insn.mnemonic,
                ' {}'.format(insn.operands) if insn.operands else ''
            ), color))

        # Add the box to the correct condition cluster subgraph.
        condition, branch_info = get_bb_condition(basic_block)
        if branch_info is None:
            label_fallthrough = label_branch = None
        else:
            condition_sloc_ranges[condition] = branch_info.cond_sloc_range
            label_fallthrough = format_edge_info(
                branch_info.edge_fallthrough, condition
            )
            label_branch = format_edge_info(
                branch_info.edge_branch, condition
            )
        add_node(pc, condition, format_html_label(label))

        # Then add outgoing edges for it.
        successors = basic_block[-1].successors
        if len(successors) == 1:
            # This is an unconditionnal jump (or a mere fallthrough).
            add_edge(pc, successors[0], None, COLOR_JUMP)
        elif len(successors) == 2:
            # This is a branch: the first one is the fallthrough, the second
            # one is the branch destination.
            add_edge(
                pc, successors[0],
                label_fallthrough, COLOR_BRANCH_FALLTHROUGH
            )
            add_edge(
                pc, successors[1],
                label_branch, COLOR_BRANCH_TAKEN
            )

    for insn in outside_insns.values():
        label = []
        for sloc in insn.slocs:
            label.append(slocinfo.format_sloc(sloc, args.basename))
        label.append('  {:#0x}'.format(insn.pc))
        add_node(insn.pc, None, format_text_label(label), shape='ellipse')

        for pc in insn.successors:
            add_edge(insn.pc, pc, None, COLOR_OUTSIDE_JUMP)

    for out_dest in (destinations - nodes):
        label = []
        for sloc in sloc_info.get(out_dest, []):
            label.append(slocinfo.format_sloc(sloc, args.basename))
        label.append('  {:#0x}'.format(out_dest))
        add_node(out_dest, None, format_text_label(label), 'ellipse')

    f = args.output
    f.write('digraph cfg {\n')
    f.write('    graph [splines=ortho]\n')

    for sco_no, nodes in by_condition.items():
        if sco_no is not None:
            f.write('subgraph cluster_condition_{} {{\n'.format(sco_no))

        # TODO: add a label for the subgraph...

        for node in nodes:
            f.write(node)
            f.write('\n')

        if sco_no is not None:
            f.write('}\n')

    for edge in edges:
        f.write(edge)
        f.write('\n')

    f.write('}\n')
