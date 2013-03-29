# -*- coding: utf-8 -*-


'''
Expose various construct that can be used to build abstract trees. Those trees
can then be used to generate programs.

The idea is not to reimplement a full AST for both C and Ada, but rather focus
on an interesting common set of constructs for tests. When too specific
constructs are needed, just use the X* node kinds: XType, XOperand, etc.
'''


from collections import namedtuple


class RelOp(object):
    '''
    Available relational operators.
    '''
    GT = '>'
    GE = '>='
    LT = '<'
    LE = '<='
    EQ = '=='
    NE = '!='

class Types(object):
    '''
    Available basic types for local variable declarations.
    '''
    BOOLEAN = 'boolean'
    INTEGER = 'integer'

class TagTypes(object):
    '''
    Available decision context types for tags.
    '''
    DECISION = ':d:'
    EXPRESSION = ':e:'


Program = namedtuple('program', 'local_vars statements')


# Types: used for both declarations and usages.
BuiltinType = namedtuple('builtin_type',    'name')
RecordType  = namedtuple('record_type',     'name members')
MemberDecl  = namedtuple('member_decl',     'type name')

# ... and some regularly used ones.
BooleanType = BuiltinType(Types.BOOLEAN)
IntegerType = BuiltinType(Types.INTEGER)
# Note that builtin types must not be declared.


# Expressions
# When adding nodes to these or to topology ones, do not forget to update the
# `utils.is_expr` function.
VariableUsage   = namedtuple('variable_usage',      'name')
LitteralInteger = namedtuple('litteral_integer',    'value')
LitteralBoolean = namedtuple('litteral_boolean',    'value')
LitteralRecord  = namedtuple('litteral_record',     'type members')
Comparison      = namedtuple('comparison',          'operator left right')
Call            = namedtuple('call',                'function arguments')

# Topology expressions
And = namedtuple('and_expr', 'left right')
Or  = namedtuple('or_expr',  'left right')
Not = namedtuple('not_expr', 'expr')


# Statements
If      = namedtuple('if_stmt',      'condition true_stmt false_stmt')
While   = namedtuple('while_stmt',   'condition stmt')
Return  = namedtuple('return_stmt',  'expr')
Assign  = namedtuple('assign',       'variable expr')

# Language-specific constructs:
# - Type
XType       = namedtuple('language_specific_type',
                         'language declaration usage')
# - Litteral (for actuals, mostly)
XLitteral   = namedtuple('language_specific_litteral',
                         'language format')
# - Operand
XOperand    = namedtuple('language_specific_operand',
                         'language format formal_name')
# - Context
XContext    = namedtuple('language_specific_context',
                         'language format decision_expr')

# Tag
# Used to tag lines, so that test drivers can reference the correct lines for
# expectations.
# - Custom tags may only define the `name` attribute, leaving `operand` and
#   `context` as None.
# - If name is `eval`, `operand` must be the formal name that is used in the
#   tagged operand, or `all` if all operands are on the same line. In both
#   cases, `context` must be `TagTypes.(DECISION or EXPRESSION)` depending on
#   the context of the tagged decision.
Tag         = namedtuple('tag',         'name operand context')
TaggedNode  = namedtuple('tagged_node', 'tag node')
