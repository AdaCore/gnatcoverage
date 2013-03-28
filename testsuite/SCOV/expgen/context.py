# -*- coding: utf-8 -*-


'''
Expose contexts suitable for decisions.
'''


import SCOV.expgen.ast as ast


class Context(object):
    '''
    Generate a program structure suitable to run the test. Generation needs a
    decision expression.
    '''

    # Subclasses must override this to `ast.TagTypes.(DECISION or EXPRESSION)`.
    TAG_CONTEXT = None

    def get_program(self, decision_expr):
        raise NotImplementedError()


class LanguageSpecific(Context):
    '''
    Language-specific context, shortcuts the AST with text substitution.

    The name of the language the context is specific to must be set in the
    `LANGUAGE` attribute.

    Subclasses have to override the `FORMAT` attribute with a list of strings.
    In order to let the engine insert the decision expression, at least one
    line should contain the "{decision_expr}" placeholder.
    '''

    LANGUAGE    = None
    TAG_CONTEXT = None
    FORMAT      = None

    def __init__(self):
        super(LanguageSpecific, self).__init__()
        self.language = self.LANGUAGE

    def get_program(self, decision_expr):
        return ast.Program(
            [], [ast.XContext(self.LANGUAGE, self.FORMAT, decision_expr)]
        )


class Call(Context):
    '''
    The decision expression is used as a parameter for a call to an "identity"
    function, and return the result of this call.

    The user must provide an implementation for this function.
    '''

    TAG_CONTEXT = ast.TagTypes.EXPRESSION

    def get_program(self, param):
        temp_name = 'result'
        temp_usage = ast.VariableUsage(temp_name)

        return ast.Program(
            [(temp_name, ast.BooleanType), ],
            [
                ast.Assign(
                    temp_usage,
                    ast.Call(ast.VariableUsage('identity'), [param])
                ),
                ast.Return(temp_usage),
            ]
        )


class If(Context):

    TAG_CONTEXT = ast.TagTypes.DECISION
    ON_TRUE_TAG = ast.Tag('on-true', None, None)
    ON_FALSE_TAG = ast.Tag('on-false', None, None)

    def get_program(self, condition):
        return ast.Program(
            [],
            [
                ast.If(
                    condition,
                    ast.TaggedNode(
                        self.ON_TRUE_TAG,
                        ast.Return(ast.LitteralBoolean(True))
                    ),
                    ast.TaggedNode(
                        self.ON_FALSE_TAG,
                        ast.Return(ast.LitteralBoolean(False))
                    )
                ),
            ]
        )

class Return(Context):

    TAG_CONTEXT = ast.TagTypes.EXPRESSION

    def get_program(self, condition):
        return ast.Program(
            [],
            [ast.Return(condition), ]
        )
