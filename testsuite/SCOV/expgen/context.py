# -*- coding: utf-8 -*-

"""Expose contexts suitable for decisions."""


import SCOV.expgen.syntax as syntax


class Context(object):
    """
    Generate a program structure suitable to run the test. Generation needs a
    decision expression.
    """

    # Subclasses must override this to `ast.TagTypes.(DECISION or EXPRESSION)`.
    TAG_CONTEXT = None

    def get_program(self, decision_expr):
        raise NotImplementedError()


class LanguageSpecific(Context):
    """Language-specific context, shortcuts the AST with text substitution.

    The name of the language the context is specific to must be set in the
    `LANGUAGE` attribute.

    Subclasses have to override the `FORMAT` attribute with a list of strings.
    In order to let the engine insert the decision expression, at least one
    line should contain the "{decision_expr}" placeholder.
    """

    LANGUAGE = None
    TAG_CONTEXT = None
    FORMAT = None

    def __init__(self):
        super(LanguageSpecific, self).__init__()
        self.language = self.LANGUAGE

    def get_program(self, decision_expr):
        return syntax.Program(
            [], [syntax.XContext(self.LANGUAGE, self.FORMAT, decision_expr)]
        )


# Tag for statements whose execution depends on the outcome of the decision
# expression.
ON_TRUE_TAG = syntax.Tag("on-true", None, None)
ON_FALSE_TAG = syntax.Tag("on-false", None, None)


class Call(Context):
    """
    The decision expression is used as a parameter for a call to an "identity"
    function, and return the result of this call.

    The user must provide an implementation for this function.
    """ ""

    TAG_CONTEXT = syntax.TagTypes.EXPRESSION

    def get_program(self, param):
        temp_name = "result"
        temp_usage = syntax.VariableUsage(temp_name)

        return syntax.Program(
            [
                (temp_name, syntax.BooleanType),
            ],
            [
                syntax.Assign(
                    temp_usage,
                    syntax.Call(syntax.VariableUsage("identity"), [param]),
                ),
                syntax.Return(temp_usage),
            ],
        )


class If(Context):
    """
    The decision expression is used as the controlling expression for an IF
    statement.
    """

    TAG_CONTEXT = syntax.TagTypes.DECISION

    def get_program(self, condition):
        return syntax.Program(
            [],  # No local variable
            [
                syntax.If(
                    condition,
                    syntax.TaggedNode(
                        ON_TRUE_TAG,
                        syntax.Return(syntax.LitteralBoolean(True)),
                    ),
                    syntax.TaggedNode(
                        ON_FALSE_TAG,
                        syntax.Return(syntax.LitteralBoolean(False)),
                    ),
                )
            ],
        )


class While(Context):
    """
    The decision expression is used as the controlling expression for a WHILE
    statement.
    """

    TAG_CONTEXT = syntax.TagTypes.DECISION

    def get_program(self, condition):
        return syntax.Program(
            [],  # No local variable
            [
                syntax.While(
                    condition,
                    syntax.TaggedNode(
                        ON_TRUE_TAG,
                        syntax.Return(syntax.LitteralBoolean(True)),
                    ),
                ),
                syntax.TaggedNode(
                    ON_FALSE_TAG, syntax.Return(syntax.LitteralBoolean(False))
                ),
            ],
        )


class Return(Context):
    TAG_CONTEXT = syntax.TagTypes.EXPRESSION

    def get_program(self, condition):
        return syntax.Program(
            [],
            [
                syntax.Return(condition),
            ],
        )
