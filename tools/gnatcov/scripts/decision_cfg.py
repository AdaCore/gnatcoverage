#! /usr/bin/env python
# -*- coding: utf-8 -*-

import argparse
import collections
import os
import re
import struct
import subprocess

import bddinfo
import intervalmap
import slocinfo
import syminfo
import traceinfo

JUMP, FALLTHROUGH, BRANCH = range(3)

STYLE_NONE                  = 'solid'
STYLE_BRANCH_FALLTHROUGH    = 'dashed'
STYLE_BRANCH_TAKEN          = 'dotted'
COLOR_COVERED               = '"#008000"'
COLOR_NOT_COVERED           = '"#a00000"'
COLOR_UNCOVERABLE           = '"#c0c0c0"'
COLOR_WARNING               = '"#ff8000"'
COLOR_NONE                  = '"#000000"'

STYLES = {
    JUMP:        STYLE_NONE,
    FALLTHROUGH: STYLE_BRANCH_FALLTHROUGH,
    BRANCH:      STYLE_BRANCH_TAKEN,
}

SLOC_RANGE = re.compile(
    '^([^:]*):(\d+):(\d+)-(\d+):(\d+)$'
)
SlocRange = collections.namedtuple('SlocRange',
    'filename'
    ' start_line start_column'
    ' end_line end_column'
)
AddressRange = collections.namedtuple('AddressRange', 'low high')
AroundAddress = collections.namedtuple('AroundAddress', 'pc')
Program = collections.namedtuple('Program', 'filename arch')
# A program is the `filename` ELF file, and when disassembled, it can contain
# the inconditional `jumps` instructions and the `branches` instructions.

# Decoder for the e_machine ELF header field. Depend on the EI_DATA header
# field.
ARCH_STRUCT = {
    1: struct.Struct('<H'),
    2: struct.Struct('>H'),
}

OBJDUMP_INSN = re.compile(
    '^[ ]*(?P<pc>[0-9a-f]+):'
    '\t[0-9a-f ]+\t'
    '(?P<mnemonic>[^ ]+)'
    '[ ]?(?:[ ]+(?P<operands>.+))?\n$'
)

OBJDUMP_DEST = re.compile('^(?P<pc>[0-9a-f]+) <(?P<symbol>[^>]+)>$')

def does_raise_exception(symbol):
    """Return if `symbol` is used to raise an exception."""
    if symbol is None:
        return False
    return (
        symbol in ('__gnat_last_chance_handler', '__gnat_raise_exception')
        or symbol.startswith('__gnat_rcheck_')
    )

class Toolchain(object):
    """Manage access to toolchain programs."""

    def __init__(self, prefix=None):
        self.prefix = prefix
        self.objdump = self.get('objdump')

    def get(self, program):
        """Return the program name including the toolchain prefix."""
        return (
            '{}-{}'.format(self.prefix, program)
            if self.prefix else
            program
        )

class Arch(object):
    CALL = 'call'
    RET = 'ret'
    COND_RET = 'cond-ret'
    JUMP = 'jump'
    BRANCH = 'branch'

    @staticmethod
    def get_insn_properties(insn):
        """Return:
            - (Arch.CALL, <subroutine addr>, <subroutine symbol>) for call
              instructions
            - (Arch.RET, None, None) for return instructions
            - (Arch.COND_RET, None, None) for conditional return instructions
            - (Arch.JUMP, <jump addr>, <symbol>) for unconditionnal jump
              instructions
            - (Arch.BRANCH, <branch addr>, <symbol>) for conditionnal branch
              instructions
            - (None, None, None) for all other instructions.
        When the destination address in unknown, None can be returned instead.
        """
        raise NotImplementedError()

class ArchX86(Arch):
    CALLS = set('call callq'.split())
    RETS = set('ret retl retq'.split())
    JUMPS = set('jmp jmpl jmpq'.split())
    BRANCHES = set(
        'ja jae jb jbe jc jcxz jecxz je jg jge jl jle jna jnae jnb jnbe jnc'
        ' jne jng jnge jnl jnle jno jnp jns jnz jo jp jpe jpo js jz'.split()
    )

    @staticmethod
    def get_insn_dest(insn):
        if insn.operands.startswith('*'):
            # TODO: handle rip-relative jumps
            return (None, None)
        else:
            m = OBJDUMP_DEST.match(insn.operands)
            if m:
                return (
                    int(m.group('pc'), 16),
                    m.group('symbol')
                )
            else:
                return (None, None)

    @staticmethod
    def get_insn_properties(insn):
        if insn.mnemonic in ArchX86.RETS:
            return (Arch.RET, None, None)
        elif insn.mnemonic in ArchX86.JUMPS:
            return (Arch.JUMP, ) + ArchX86.get_insn_dest(insn)
        elif insn.mnemonic in ArchX86.BRANCHES:
            return (Arch.BRANCH, ) + ArchX86.get_insn_dest(insn)
        elif insn.mnemonic in ArchX86.CALLS:
            _, symbol = ArchX86.get_insn_dest(insn)
            return (Arch.CALL, None, symbol)
        else:
            return (None, None, None)

class ArchPPC32(Arch):
    @staticmethod
    def get_insn_dest(operand):
        m = OBJDUMP_DEST.match(operand)
        if m:
            return (
                int(m.group('pc'), 16),
                m.group('symbol')
            )
        else:
            return (None, None)

    @staticmethod
    def get_insn_properties(insn):
        if not insn.mnemonic.startswith('b'):
            return (None, None, None)

        # Strip prediction any hint.
        mnemonic = insn.mnemonic.rstrip('+-')

        # Branch can go to an absolute address, to the link register or to the
        # control register.
        dest_in_reg = False
        return_from_subroutine = False
        if mnemonic.endswith('l') or mnemonic.endswith('la'):
            # Branch and Link (call)
            if 'ctr' in mnemonic:
                # To ConTrol Register (destination known at runtime)
                symbol = None
            elif mnemonic.startswith('blr'):
                # To Link Register (anyway this is a call, *not* a return)
                symbol = None
            else:
                _, symbol = ArchPPC32.get_insn_dest(insn.operands.split(',')[0])
            return (Arch.CALL, None, symbol)
        elif mnemonic.endswith('lr'):
            # To Link Register (return)
            if mnemonic != 'blr':
                return (Arch.COND_RET, None, None)
            else:
                return (Arch.RET, None, None)
        elif mnemonic.endswith('ctr'):
            # To ConTrol Register (destination known at runtime)
            return (Arch.BRANCH, None, None)
        elif mnemonic.endswith('a'):
            mnemonic = mnemonic[:-1]

        operands = insn.operands.split(',')
        if ' ' in operands[0]:
            pc, symbol = ArchPPC32.get_insn_dest(operands[0])
        else:
            pc, symbol = ArchPPC32.get_insn_dest(operands[1])

        return (
            Arch.BRANCH if mnemonic != 'b' else Arch.JUMP,
            pc, symbol
        )

class ArchSPARC32(Arch):
    @staticmethod
    def get_insn_dest(operand):
        m = OBJDUMP_DEST.match(operand)
        if m:
            return (
                int(m.group('pc'), 16),
                m.group('symbol')
            )
        else:
            return (None, None)

    @staticmethod
    def get_insn_properties(insn):
        if insn.mnemonic.startswith('b') and insn.mnemonic != 'b':
            addr, symbol = ArchSPARC32.get_insn_dest(
                insn.operands.split(',')[0]
            )
            return (Arch.BRANCH, addr, symbol)
        elif insn.mnemonic in ('jmp', 'b'):
            addr, symbol = ArchSPARC32.get_insn_dest(
                insn.operands.split(',')[0]
            )
            return (Arch.JUMP, addr, symbol)
        elif insn.mnemonic == 'call':
            addr, symbol = ArchSPARC32.get_insn_dest(
                insn.operands.split(',')[0]
            )
            return (Arch.CALL, addr, symbol)
        elif insn.mnemonic == 'ret':
            return (Arch.RET, None, None)
        else:
            return (None, None, None)

ARCHITECTURES = {
    # SPARC 32bit
    2:  ArchSPARC32,
    # x86
    3:  ArchX86,
    # PowerPC 32bit
    20: ArchPPC32,
    # x86_64
    62: ArchX86,
}

def which(program):
    """Return whether `program` is in the PATH."""
    with open(os.devnull, 'rb+') as devnull:
        proc = subprocess.Popen(
            ['which', program], stdin=devnull, stdout=devnull
        )
        proc.wait()
        return proc.returncode == 0

def parse_target(string):
    """Check `string` is a valid toolchain prefix and return toolchain."""
    toolchain = Toolchain(string)
    if not which(toolchain.objdump):
        raise argparse.ArgumentTypeError(
            'No {} found'.format(toolchain.objdump)
        )
    return toolchain

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

def parse_address_range(string):
    chunks = string.split('..')
    if len(chunks) != 2:
        raise argparse.ArgumentTypeError(
            'Invalid address range: {}'.format(string)
        )
    low, high = chunks
    try:
        low = int(low, 16)
    except ValueError:
        raise argparse.ArgumentTypeError(
            'Invalid hexadecimal low address: {}'.format(low)
        )
    try:
        high = int(high, 16)
    except ValueError:
        raise argparse.ArgumentTypeError(
            'Invalid hexadecimal high address: {}'.format(low)
        )
    return AddressRange(low, high)

def parse_around_address(string):
    pc_string = string[1:]
    try:
        pc = int(pc_string, 16)
    except ValueError:
        raise argparse.ArgumentTypeError(
            'Invalid hexadecimal address: {}'.format(pc_string)
        )
    else:
        return AroundAddress(pc)

def parse_location(string):
    if ':' in string:
        return parse_sloc_range(string)
    elif string.startswith('@'):
        return parse_around_address(string)
    elif '..' in string:
        return parse_address_range(string)
    else:
        return syminfo.Symbol(None, None, string)

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

class Locations(object):
    """Gather information about code matching criterias."""

    def __init__(
        self, symbols,
        matched_symbols,
        matched_addr_ranges,
        matched_sloc_ranges
    ):
        self.symbols = symbols
        self.matched_symbols = set(matched_symbols)
        self.matched_addresses = intervalmap.IntervalMap()
        for low, high in matched_addr_ranges:
            self.matched_addresses[low:high] = True
        # TODO: matching slocs may be more efficient using an interval map. The
        # only thing to do is to flatten the given list... (interval maps do
        # not handle overlapping elements)
        self.matched_sloc_ranges = matched_sloc_ranges

    def match(self, slocs, address):
        """Return if any sloc or the symbol corresponding to the
        given address is matched by criterias.
        """
        try:
            symbol = self.symbols[address].name
        except KeyError:
            symbol = None
        if symbol in self.matched_symbols:
            return True
        if address in self.matched_addresses:
            return True
        for sloc_range in self.matched_sloc_ranges:
            if slocs_match_range(slocs, sloc_range):
                return True
        return False


class Insn:
    """A single instruction. It knows if it ends a basic block and which are
    its execution successors."""
    def __init__(self, pc, mnemonic, operands, slocs=None):
        self.pc = pc
        self.next_pc = None
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

class EdgesSet:

    def __init__(self):
        # Mapping: source PC -> set of destinations PC
        self.edges = {}

    def add(self, edge):
        src, dest = edge
        try:
            dest_set = self.edges[src]
        except KeyError:
            dest_set = self.edges[src] = set()
        dest_set.add(dest)

    def __contains__(self, edge):
        src, dest = edge
        try:
            return dest in self.edges[src]
        except KeyError:
            return False


def get_decision_cfg(program, toolchain, sloc_info, locations):
    get_insn_properties = program.arch.get_insn_properties

    # Let objdump disassemble the program for us...
    args = [toolchain.objdump, '-d', program.filename]
    print('Disassembling: {}'.format(args))
    proc = subprocess.Popen(
        args,
        stdin=open(os.devnull, 'rb'), stdout=subprocess.PIPE
    )

    # ... and filter instructions in the given sloc range.
    # This list will contain the list of instructions we are interested in.
    instructions = []
    outside_instructions = {}
    uncoverable_edges = EdgesSet()
    # And this will contain addresses of instructions that must start a basic
    # block.
    basic_block_starters = set()
    # True if and only if the last visited instruction can fallthrough the
    # current one.
    last_instruction_can_fallthrough = False
    # True if the last visited instruction is supposed to raise an exception.
    last_instruction_raises_exception = False
    # True if and only if the last visited instruction was inside the decision.
    last_instruction_in_decision = False
    last_instruction = None

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
        if last_instruction:
            last_instruction.next_pc = pc
        sloc_in_decision = locations.match(insn.slocs, pc)

        # This instruction is the successor of the previous instruction.
        if last_instruction_can_fallthrough:
            last_instruction.add_successor(insn.pc, first=True)
            if sloc_in_decision and not last_instruction_in_decision:
                # Here, the previous instruction was not in the decision, but
                # we are interested in it anyway.
                outside_instructions[last_instruction.pc] = last_instruction

        # Add this instruction if it belongs to the decision.
        if sloc_in_decision:
            instructions.append(insn)
        elif last_instruction_can_fallthrough and last_instruction_in_decision:
            # Out of the decision: end the previous basic block if needed.
            instructions[-1].end_basic_block()

        if (
            last_instruction_raises_exception and
            (sloc_in_decision or last_instruction_in_decision)
        ):
            uncoverable_edges.add((last_instruction.pc, pc))

        # If this is a jump/branch, it ends its own basic block and it must
        # break some other basic block.
        insn_type, dest, dest_symbol = get_insn_properties(insn)
        raises_exception = does_raise_exception(dest_symbol)
        if insn_type in (Arch.JUMP, Arch.BRANCH, Arch.COND_RET):
            insn.add_successor(dest, end_basic_block=True)

            if dest is not None:
                dest_slocs = sloc_info.get(dest, [])

                # Remember the jump/branch destination must start a basic block
                # only in the case it is inside the decision.
                if locations.match(dest_slocs, dest):
                    basic_block_starters.add(dest)
                    # If the current instruction is outside, remember it
                    # anyway.
                    if not sloc_in_decision:
                        outside_instructions[insn.pc] = insn
        elif insn_type == Arch.RET or raises_exception:
            insn.end_basic_block()

        # Update "last_*" information for the next iteration.
        last_instruction_can_fallthrough = (
            insn_type not in (Arch.RET, Arch.JUMP)
        )
        last_instruction_raises_exception = raises_exception
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
    return cfg, uncoverable_edges, outside_instructions


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
        '--target', dest='toolchain', type=parse_target, default=None,
        help=(
            'Prefix used to reach the toolchain'
            ' (example: powerpc-elf for powerpc-elf-objdump)'
        )
    )
    parser.add_argument(
        '-T', '--format', default=None,
        help='If given, call dot to produce the actual output passing it'
        ' this argument'
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
        '-k', '--keep-uncoverable-edges', dest='keep_uncoverable_edges',
        action='store_true',
        help='Do not strip edges that are supposed to be uncoverable due to'
        ' exceptions'
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
        'location', type=parse_location, nargs='+',
        help=(
            'Location of the decision to analyse.'
            ' Can be a sloc range (example: 10:5-11:21),'
            ' a symbol name (example: ada__text_io__put_line__2),'
            ' an address range (example: 0x200..0x300)'
            ' or the symbol around some address (example: @0x0808f31a)'
        )
    )

    args = parser.parse_args()

    # Create the default toolchain only when needed, since it may raise an
    # exception when some tool is not available.
    if args.toolchain is None:
        args.toolchain = parse_target(None)

    # If asked to, start dot to format the output.
    if args.format:
        with open(os.devnull, 'wb') as devnull:
            dot_process = subprocess.Popen(
                ['dot', '-T{}'.format(args.format), '-o', args.output.name],
                stdin=subprocess.PIPE, stdout=devnull
            )
        args.output.close()
        f = dot_process.stdin
    else:
        f = args.output

    sym_info, overlap_syms = syminfo.get_sym_info(args.program.filename)

    if overlap_syms:
        sys.stderr.write('warning: some symbols overlap with others:\n')
        for sym in overlap_syms:
            sys.stderr.write('  - {}\n'.format(syminfo.format_symbol(sym)))

    # Build accepted locations
    accepted_symbols = []
    accepted_slocs = []
    accepted_addr_ranges = []
    for loc in args.location:
        if isinstance(loc, syminfo.Symbol):
            accepted_symbols.append(loc.name)
        elif isinstance(loc, AddressRange):
            accepted_addr_ranges.append(loc)
        elif isinstance(loc, SlocRange):
            accepted_slocs.append(loc)
        elif isinstance(loc, AroundAddress):
            try:
                symbol = sym_info[loc.pc]
            except KeyError:
                sys.stderr.write('No symbol around: {:#08x}\n'.format(loc.pc))
                sys.exit(1)
            accepted_symbols.append(symbol.name)
        else:
            # We are not supposed to end up here since args.location items come
            # from arguments parsing.
            assert False

    locations = Locations(
        sym_info,
        accepted_symbols,
        accepted_addr_ranges,
        accepted_slocs
    )

    sloc_info = slocinfo.get_sloc_info(args.program.filename)
    decision_cfg, uncoverable_edges, outside_insns = get_decision_cfg(
        args.program, args.toolchain,
        sloc_info, locations
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
    executed_insns, leave_flags = (
        traceinfo.get_trace_info(args.traces)
        if args.traces is not None else
        (None, None)
    )
    trace_info = executed_insns is not None

    # Use the BDD, and especially its EXCEPTION edges info to tag as
    # uncoverable the successors of basic blocks that raise an exception.
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
                    last_insn = next_basic_block[-1]
                    for successor in last_insn.successors:
                        uncoverable_edges.add((last_insn.pc, successor))

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

    def format_edge_color(branch_insn, kind, uncoverable):
        if trace_info:
            if kind == JUMP:
                covered = branch_insn.pc in executed_insns
            else:
                try:
                    flags = leave_flags[branch_insn.next_pc]
                except KeyError:
                    covered = False
                else:
                    covered = (
                        (kind == FALLTHROUGH and flags.fallthrough)
                        or (kind == BRANCH and flags.branch)
                    )
        else:
            covered = False
        return (
            (COLOR_WARNING if uncoverable else COLOR_COVERED)
            if covered else
            (COLOR_UNCOVERABLE if uncoverable else COLOR_NOT_COVERED)
        )

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

    def add_edge(from_, to, label, color=COLOR_NONE, style=STYLE_NONE):
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
        edges.append('    {} -> {} [{}color={},style={},penwidth=3];'.format(
            pc_to_name(from_), to_name,
            'label={}, '.format(format_text_label(label)) if label else '',
            color, style
        ))

    def add_node(pc, condition, label, shape='box', unknown=False):
        by_condition[condition].append(
            '    {} [shape={}, fontname=monospace, label={}];\n'.format(
                pc_to_name(pc, unknown), shape, label
            )
        )
        nodes.add(pc)

    def process_successor_edges(
        from_pc, insn,
        label_fallthrough,
        label_branch
    ):
        def process_edge(kind, to_pc, label):
            uncoverable = (insn.pc, to_pc) in uncoverable_edges
            if args.keep_uncoverable_edges or not uncoverable:
                add_edge(
                    from_pc, to_pc, label,
                    format_edge_color(insn, kind, uncoverable),
                    STYLES[kind]
                )

        successors = insn.successors
        if len(successors) == 1:
            # This is an unconditionnal jump (or a mere fallthrough).
            process_edge(JUMP, successors[0], None)
        elif len(successors) == 2:
            # This is a branch: the first one is the fallthrough, the second
            # one is the branch destination.
            process_edge(FALLTHROUGH, successors[0], label_fallthrough)
            process_edge(BRANCH, successors[1], label_branch)

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
                    if insn.pc in executed_insns else
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
        process_successor_edges(
            pc, basic_block[-1],
            label_fallthrough,
            label_branch
        )

    for insn in outside_insns.values():
        label = []
        for sloc in insn.slocs:
            label.append(slocinfo.format_sloc(sloc, args.basename))
        label.append('  {:#0x}'.format(insn.pc))
        add_node(insn.pc, None, format_text_label(label), shape='ellipse')
        process_successor_edges(insn.pc, insn)

    for out_dest in (destinations - nodes):
        label = []
        for sloc in sloc_info.get(out_dest, []):
            label.append(slocinfo.format_sloc(sloc, args.basename))
        label.append('  {:#0x}'.format(out_dest))
        add_node(out_dest, None, format_text_label(label), 'ellipse')

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
    f.close()
