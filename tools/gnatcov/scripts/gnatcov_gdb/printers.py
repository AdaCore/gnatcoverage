"""
Collection of pretty-printers for types in gnatcov.
"""

import gdb
import gdb.printing

from gnatdbg.records import decoded_record
from gnatdbg.sets import OrderedSetPrinter
from gnatdbg.strings import StringAccess


class SubPrettyPrinter(gdb.printing.SubPrettyPrinter):
    """
    Base class for all type-specific pretty-printers.
    """

    enabled = True

    name = None
    """
    Fully-qualified name for the type this pretty-printer handles.
    """

    @classmethod
    def type_name(cls):
        return cls.name.replace('.', '__').lower()

    @classmethod
    def matches(cls, value):
        return value.type.name == cls.type_name()

    def __init__(self, value):
        self.value = value


class PCTypePrinter(SubPrettyPrinter):
    """
    Pretty-printer for Traces.Pc_Type.
    """

    name = 'Traces.Pc_Type'

    def to_string(self):
        return hex(int(self.value))


class ArchAddrPrinter(SubPrettyPrinter):
    """
    Pretty-printer for Arch.Arch_Addr.
    """

    name = 'Arch.Arch_Addr'

    def to_string(self):
        return hex(int(self.value))


class LocalSlocPrinter(SubPrettyPrinter):
    """
    Pretty-printer for Slocs.Local_Source_Location.
    """

    name = 'Slocs.Local_Source_Location'

    def to_string(self):
        return '{}:{}'.format(self.value['line'], self.value['column'])


class SlocPrinter(SubPrettyPrinter):
    """
    Pretty-printer for Slocs.Source_Location.
    """

    name = 'Slocs.Source_Location'

    def to_string(self):
        return '[file {}] {}'.format(self.value['source_file'],
                                     self.value['l'])


class SCOId(SubPrettyPrinter):
    """
    Pretty-printer for SC_Obligations.SCO_Id.
    """

    name = 'SC_Obligations.SCO_Id'

    def to_string(self):
        return 'SCO#{}'.format(int(self.value))


class AddressInfo(SubPrettyPrinter):
    """
    Pretty-printer for Traces_Elf.Address_Info_Acc.
    """

    name = 'Traces_Elf.Address_Info_Acc'

    def to_string(self):
        if not self.value:
            return 'null'

        v = decoded_record(self.value.dereference())

        kind = str(v['kind'])
        prefix = '{} ({}..{})'.format(
            '_'.join(k.capitalize() for k in kind.split('_')),
            v['first'], v['last']
        )
        if kind == 'compilation_unit_addresses':
            suffix = 'DIE_CU: {}'.format(v['die_cu'])

        elif kind == 'section_addresses':
            name = StringAccess(v['section_name'])
            suffix = '{} [index: {}]'.format(name.get_string(),
                                             v['section_sec_idx'])

        elif kind == 'subprogram_addresses':
            suffix = repr(StringAccess(v['subprogram_name']).get_string())

        elif kind == 'inlined_subprogram_addresses':
            suffix = 'call at {}'.format(v['call_sloc'])

        elif kind == 'symbol_addresses':
            suffix = repr(StringAccess(v['symbol_name']).get_string())

        elif kind == 'line_addresses':
            suffix = str(v['sloc'])
            if v['disc']:
                suffix += ' [disc: {}]'.format(v['disc'])

        else:
            suffix = '<invalid kind>'

        return '{} => {}'.format(prefix, suffix)


class InsnSetRangesPrinter(SubPrettyPrinter):
    """
    Pretty-printer for Elf_Disassemblers.Insn_Set_Ranges.
    """

    name = 'Elf_Disassemblers.Insn_Set_Ranges'

    @property
    def set_pp(self):
        return OrderedSetPrinter(self.value['_parent'])

    def to_string(self):
        return self.set_pp.to_string()

    def children(self):
        return self.set_pp.children()


class InsnSetRangePrinter(SubPrettyPrinter):
    """
    Pretty-printer for Elf_Disassemblers.Insn_Set_Range
    """

    name = 'Elf_Disassemblers.Insn_Set_Range'

    def to_string(self):
        return '{:0x} .. {:0x} => {}'.format(
            int(self.value['first']), int(self.value['last']),
            self.value['insn_set']
        )


class PrettyPrinter(gdb.printing.PrettyPrinter):
    def __call__(self, value):
        for p in self.subprinters:
            if p.enabled and p.matches(value):
                return p(value)


def register_printers(objfile):
    gdb.printing.register_pretty_printer(objfile, PrettyPrinter('gnatcov', [
        ArchAddrPrinter,
        PCTypePrinter,
        SlocPrinter,
        LocalSlocPrinter,
        SCOId,
        AddressInfo,
        InsnSetRangesPrinter,
        InsnSetRangePrinter,
    ]))
