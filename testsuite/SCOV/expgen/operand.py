# -*- coding: utf-8 -*-


'''
Expose operand kinds suitable for condition expressions.
'''


import SCOV.expgen.ast as ast


class Operand(object):
    '''
    A kind of operand for conditions: a variable, a comparison with an integer,
    a function call, etc.
    '''

    def __init__(self, used_types, param_type, actuals):
        # Set for all the types used by the operand kind. They will be used by
        # the serializer to output type declarations.
        self.used_types = used_types

        # Type of the formal used in the operand. Must be in `used_types`.
        self.param_type = param_type

        # Two litterals that make the operand evaluate to False/True.
        self.actuals = actuals

    def get_operand(self, param):
        '''
        Turn some parameter name into an abstract tree for the operand.
        '''
        raise NotImplementedError()


class LanguageSpecific(Operand):
    '''
    Language-specific operand, shortcuts the AST with text substitution.

    The name of the language the operand is specific to must be set in the
    `LANGUAGE` attribute.

    Subclasses have to override the `FORMAT` attribute with a string template
    of the operand expression. In order to let the engine insert the formal
    name used in the operand expression, the template should contain the
    "{formal_name}" placeholder.

    Subclasses also have to override the `USED_TYPES`, `PARAM_TYPE` and
    `ACTUALS` attributes.

    TODO: document more deeply those attributes.
    '''

    LANGUAGE    = None

    FORMAT      = None
    USED_TYPES  = None
    PARAM_TYPE  = None
    ACTUALS     = None

    def __init__(self):
        self.language = self.LANGUAGE

        # Convert USED_TYPES to ast.XType nodes.
        xtypes = []
        param_type = None
        for type_ in self.USED_TYPES:
            xtype = self.convert_type(type_)
            xtypes.append(xtype)

            # Pick the one that is PARAM_TYPE instead of generating PARAM_TYPE
            # itself, so that in the end, PARAM_TYPE still is in USED_TYPES.
            if type_ is self.PARAM_TYPE:
                param_type = xtype

        assert param_type is not None, (
            'PARAM_TYPE must be present in USED_TYPES'
        )

        # Convert ACTUALS to ast.XLitteral nodes.
        actuals = {
            False:  ast.XLitteral(self.LANGUAGE, self.ACTUALS[False]),
            True:   ast.XLitteral(self.LANGUAGE, self.ACTUALS[True]),
        }

        super(LanguageSpecific, self).__init__(xtypes, param_type, actuals)

    def get_operand(self, param):
        return ast.XOperand(self.LANGUAGE, self.FORMAT, param)


    # Mapping: type_tuple id -> ast.XType
    # This is global since a type can be used in more than one operand kind,
    # and each operand kind that contain this type can appear in the same
    # program.
    converted_types = {}

    def convert_type(self, type_):
        type_id = id(type_)
        try:
            xtype = self.converted_types[type_id]
        except KeyError:
            declaration, usage = type_
            xtype = ast.XType(self.LANGUAGE, declaration, usage)
            self.converted_types[type_id] = xtype
        return xtype



class Variable(Operand):
    '''
    The operand is just the usage of a boolean argument.
    '''

    ACTUALS = {
        False: ast.LitteralBoolean(False),
        True: ast.LitteralBoolean(True),
    }

    def __init__(self):
        super(Variable, self).__init__(
            (ast.BooleanType, ),
            ast.BooleanType,
            self.ACTUALS
        )

    def get_operand(self, param):
        return ast.VariableUsage(param)

class IntegerComparison(Operand):
    '''
    Operand that compares the parameter with a given integer.
    '''

    # Provide a factory for the actuals for each relationnal operator.
    # Each factory takes the compared integer and returns (a false actual, a
    # true one).
    ACTUALS_MAKERS = {
        ast.RelOp.GT:
            lambda value: (value - 1, value + 1),
        ast.RelOp.GE:
            lambda value: (value - 1, value + 1),
        ast.RelOp.LT:
            lambda value: (value + 1, value - 2),
        ast.RelOp.LE:
            lambda value: (value + 1, value - 2),
        ast.RelOp.EQ:
            lambda value: (value + 1, value),
        ast.RelOp.NE:
            lambda value: (value, value + 1),
    }

    def __init__(self, operator, value):
        actual_false, actual_true = self.ACTUALS_MAKERS[operator](value)
        super(IntegerComparison, self).__init__(
            (ast.IntegerType, ),
            ast.IntegerType,
            {
                False:  ast.LitteralInteger(actual_false),
                True:   ast.LitteralInteger(actual_true),
            }
        )
        self.operator = operator
        self.value = ast.LitteralInteger(value)

    def get_operand(self, param):
        return ast.Comparison(
            self.operator,
            ast.VariableUsage(param),
            self.value
        )
