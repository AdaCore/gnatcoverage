# -*- coding: utf-8 -*-

import collections
import re
import subprocess


# These pattern expect no error from gnatcov output. They make a lot of
# assumptions over its format.
COND_LINE = re.compile(
    b'^---.*@(?P<insn_pc>[0-9a-f]+) .*: notice:'
    b' cond branch for SCO #(?P<sco_no>\d+):'
    b' CONDITION at (?P<sloc_range>.*) \(.*\)'
    b'$'
)
EDGES_LINE = re.compile(
    b'^---.*@(?P<insn_pc>[0-9a-f]+) .*: notice:'
    b' (?P<branch_cond_eval>.*)->BRANCH'
    b' = [0-9a-f]+ (?P<branch_dest_kind>.*)'
    b' /'
    b' (?P<fallthrough_cond_eval>.*)->FALLTHROUGH'
    b' = [0-9a-f]+ (?P<fallthrough_dest_kind>.*)'
    b'$'
)


# Destination kinds for edges in a decision.
DestCondition = collections.namedtuple('DestCondition', 'sco_no')
DestOutcome = collections.namedtuple('DestOutcome', 'value')
DestRaiseException = collections.namedtuple('DestRaiseException', '')
DestUnknown = collections.namedtuple('DestUnknown', '')

# Information about an edge that leaves a basic block because of a branch.
EdgeInfo = collections.namedtuple('EdgeInfo',
    # What the edge determine about the evaluation of the condition:
    #   Unknown -> None, True or False
    'cond_eval'
    # Destination kind for the edge.
    ' dest_kind'
)

BranchInfo = collections.namedtuple('BranchInfo',
    # SCO number and sloc for the corresponding condition
    'cond_sco_no'
    ' cond_sloc_range'
    # Edge information for the fallthrough/branch destinations.
    ' edge_fallthrough'
    ' edge_branch'
)


def parse_edge_info(raw_cond_eval, raw_dest_kind):
    """Parse edge info for given fields. Return an EdgeInfo field."""

    cond_eval = {
        'UNKNOWN': None,
        'FALSE': False,
        'TRUE': True,
    }[raw_cond_eval]

    dest_chunks = raw_dest_kind.split(' ')
    kind = dest_chunks[0]
    if kind == 'CONDITION':
        # raw_dest_kind is like 'CONDITION (12)': remove parents
        sco_no = int(dest_chunks[1][1:-1])
        dest_kind = DestCondition(sco_no)
    elif kind == 'OUTCOME':
        # raw_dest_kind is like 'OUTCOME (TRUE)': remove parents
        value = {
            'UNKNOWN': None,
            'FALSE': False,
            'TRUE': True,
        }[dest_chunks[1][1:-1]]
        dest_kind = DestOutcome(value)
    elif kind == 'RAISE_EXCEPTION':
        dest_kind = DestRaiseException()
    elif kind == 'UNKNOWN':
        dest_kind = DestUnknown()
    else:
        raise ValueError(
            'Invalid edge destination kind: {}'.format(raw_dest_kind)
        )

    return EdgeInfo(cond_eval, dest_kind)


def get_bdd_info(exe_filename, scos):
    """Parse BDD info in `exe_filename` using its `scos`. Return a map from
    branch addresses to conditions SCO numbers and edges information.
    """

    # Let gnatcov build the BDD for us.
    proc = subprocess.Popen(
        [
            'gnatcov', 'map-routines', '-v',
            '--scos={}'.format(scos), exe_filename,
        ], stdout=subprocess.PIPE,
    )
    outs, errs = proc.communicate()
    if proc.returncode != 0:
        raise RuntimeError('gnatcov map-routines returned an error')

    # Parse its output and collect information.
    cond_sco_nos = {}
    edge_infos = {}
    for line in outs.split(b'\n'):

        m = COND_LINE.match(line)
        if m:
            pc = int(m.group('insn_pc'), 16)
            cond_sco_nos[pc] = (
                int(m.group('sco_no')),
                m.group('sloc_range')
            )
            continue

        m = EDGES_LINE.match(line)
        if m:
            pc = int(m.group('insn_pc'), 16)
            edge_infos[pc] = (
                # Fallthrough edge
                parse_edge_info(
                    m.group('fallthrough_cond_eval'),
                    m.group('fallthrough_dest_kind')
                ),
                # Branch edge
                parse_edge_info(
                    m.group('branch_cond_eval'),
                    m.group('branch_dest_kind'),
                )
            )
            continue

    def get_edge_info(pc):
        try:
            return edge_infos[pc]
        except KeyError:
            # When gnatcov has trouble with its branch analysis, its output can
            # lack some edge info.
            return None, None

    return {
        pc: BranchInfo(cond_sco_no, cond_sloc_range, *get_edge_info(pc))
        for pc, (cond_sco_no, cond_sloc_range) in cond_sco_nos.items()
    }

if __name__ == '__main__':
    import sys
    for pc, br_info in get_bdd_info(sys.argv[1], sys.argv[2]).items():
        print('{:x}: SCO #{} {}'.format(
            pc, br_info.cond_sco_no, br_info.cond_sloc_range
        ))
        print('    fallthrough: {}'.format(br_info.edge_fallthrough))
        print('    branch     : {}'.format(br_info.edge_branch))
