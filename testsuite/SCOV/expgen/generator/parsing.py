# -*- coding: utf-8 -*-

'''
Code is generated depending on information extracted from test drivers. This
module provides various helpers to parse them.
'''

import SCOV.expgen.ast      as ast
import SCOV.expgen.topology as topology

from SCOV.expgen.generator.errors import DriverError, BadTopologyError


def parse_topology(topo_dir, driver, line_no, tokens):
    '''
    Parse a topology, which look like an Ada boolean expression with '_' for
    operand placeholders (and a space between each token). For instance:
        ( _ or else _ ) and then not _
    '''

    # Parsing is done using a very simple LL(2) grammar:
    #   topology = or
    #   or  = and ["or else" or]*
    #   and = not ["and then" not]*
    #   not = "not" not
    #       | "(" not ")"
    #       | "_"

    def error(message):
        raise BadTopologyError(topo_dir, driver, line_no, message)

    def check_eof(i):
        if i >= len(tokens):
            raise error('unexpected end of line')

    def parse_or(i):
        check_eof(i)
        left, i = parse_and(i)
        while i < len(tokens):
            if tokens[i:i + 2] != ['or', 'else']:
                raise error(
                    'expected "or else" but found {}'.format(tokens[i])
                )
            i += 2
            right, i = parse_and(i)
            left = ast.And(left, right)
        return left, i

    def parse_and(i):
        check_eof(i)
        left, i = parse_not(i)
        while i < len(tokens):
            if tokens[i:i + 2] != ['and', 'then']:
                raise error(
                    'expected "and then" but found {}'.format(tokens[i])
                )
            i += 2
            right, i = parse_not(i)
            left = ast.And(left, right)
        return left, i

    def parse_not(i):
        check_eof(i)
        if tokens[i] == 'not':
            expr, i = parse_not(i + 1)
            return ast.Not(expr), i
        elif tokens[i] == '_':
            return (topology.OperandPlaceholder(), i + 1)
        elif tokens[i] == '(':
            result, i = parse_or(i + 1)
            if i >= len(tokens):
                raise error(
                    'expected ")" but found end of line'.format(tokens[i])
                )
            elif tokens[i] != ')':
                raise error(
                    'expected ")" but found {}'.format(tokens[i])
                )
            return (result, i + 1)
        else:
            raise error(
                'expected "not", "(" or a placeholder but found '
                '{}'.format(tokens[i])
            )

    # Check that all tokens have been consumed.
    topo, next_token = parse_or(0)
    if next_token < len(tokens):
        raise error('Too much operands (parsed {})'.format(
            ' '.join(tokens[:next_token])
        ))

    return topo

def parse_truth(topo_dir, driver, line_no, c):
    '''
    Parse a truth character ('F' or 'T') and return the corresponding truth
    value, or raise an error.
    '''
    if c == 'F':
        return False
    elif c == 'T':
        return True
    else:
        raise DriverError(env.topo_dir, driver, line_no,
            'Invalid truth character: {}'.format(c)
        )

def parse_driver(topo_dir, driver, truth_vectors):
    '''
    Parse the `driver` test driver Ada source. Return the topology in it and
    fill the `truth_vectors` with run procedure calls found in it.
    '''
    drv_topo = None

    for line_no, line in enumerate(open(driver, 'r'), 1):

        # Parse a topology declaration
        if line.startswith('-- Topology: '):
            if drv_topo is not None:
                raise DriverError(topo_dir, driver, line_no,
                    'There is more than one topology'
                )
            _, topo_str = line.split(': ', 1)
            drv_topo = parse_topology(
                topo_dir, driver, line_no, topo_str.split()
            )

        # Parse a run procedure call, extract a truth vector from it.
        elif line.startswith('   Run_'):

            if drv_topo is None:
                raise DriverError(topo_dir, driver, line_no,
                    'A topology is needed before a run procedure is called'
                )

            chunks = line.split('_', 2)
            assert chunks[0] == '   Run'
            if len(chunks) != 3:
                raise DriverError(env.topo_dir, driver, line_no,
                    'Invalid run invokation: {}'.format(line.strip())
                )

            operands_truth = tuple(
                parse_truth(topo_dir, driver, line_no, c)
                for c in chunks[1]
            )
            expected_truth = parse_truth(
                topo_dir, driver, line_no, chunks[2][0]
            )
            truth_vectors.add(operands_truth + (expected_truth, ))

    return drv_topo
