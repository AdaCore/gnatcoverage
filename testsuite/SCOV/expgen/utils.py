# -*- coding: utf-8 -*-

'''
Various helpers used through the whole package.
'''

import SCOV.expgen.ast as ast


def check_tag(tag):
    '''
    Assert whether the given `tag` is valid, or not.
    '''
    if tag.name == 'eval':
        assert tag.context in (ast.TagTypes.DECISION, ast.TagTypes.EXPRESSION)
        assert tag.operand is not None
    else:
        assert tag.operand is None
        assert tag.context is None

def format_tag(tag):
    '''
    Serialize the given `tag`.
    '''
    if tag.name == 'eval':
        return '# {}-{} {}'.format(tag.name, tag.operand, tag.context)
    else:
        return '# {}'.format(tag.name)

def contains_tag(node):
    '''
    Return if the given `node` tree contains a tagged node.
    '''
    return (
        not isinstance(node, (basestring, bool, int)) and
        (
            isinstance(node, ast.TaggedNode) or
            any(contains_tag(subnode) for subnode in node)
        )
    )

def is_expr(node):
    '''
    Return whether `node` is an expression.
    '''
    return isinstance(node, (
        ast.VariableUsage,
        ast.LitteralInteger, ast.LitteralBoolean, ast.LitteralRecord,
        ast.Comparison, ast.Call,
        ast.XLitteral, ast.XOperand,
        ast.And, ast.Or, ast.Not,
    ))

def make_type_set(types):
    '''
    Return a list of types without doubles.
    '''

    result_ids = set()

    def add_if_new(type_):
        id_ = id(type_)
        if id_ not in result_ids:
            result_ids.add(id_)
            return True
        else:
            return False

    return [
        type_
        for type_ in types
        if add_if_new(type_)
    ]

def is_topology_equal(topo1, topo2):
    '''
    Return whether two topologies are equal.
    '''
    if type(topo1) != type(topo2):
        return False
    elif isinstance(topo1, (ast.And, ast.Not, ast.Or)):
        return all(
            is_topology_equal(sub_topo1, sub_topo2)
            for sub_topo1, sub_topo2 in zip(topo1, topo2)
        )
    else:
        # typo1 and typo2 are placeholders
        return True
