"""Expose contexts suitable for decisions."""

from __future__ import annotations

import abc
from typing import ClassVar

import SCOV.expgen.syntax as syntax


class Context:
    """
    Generate a program structure suitable to run the test. Generation needs a
    decision expression.
    """

    TAG_CONTEXT: ClassVar[syntax.TagTypes]

    @abc.abstractmethod
    def get_program(self, decision_expr: syntax.Expr) -> syntax.Program:
        pass


class LanguageSpecific(Context):
    """
    Language-specific context, shortcuts the AST with text substitution.
    """

    LANGUAGE: ClassVar[str]
    """
    Return the name of the language this context is specific to.
    """

    FORMAT: ClassVar[list[str]]
    """
    Return a list of strings in which to insert the decision expression
    (``{decision_expr}`` placeholder).
    """

    def get_program(self, decision_expr: syntax.Expr) -> syntax.Program:
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

    def get_program(self, param: syntax.Expr) -> syntax.Program:
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

    def get_program(self, condition: syntax.Expr) -> syntax.Program:
        return syntax.Program(
            [],  # No local variable
            [
                syntax.If(
                    condition,
                    syntax.TaggedStmt(
                        ON_TRUE_TAG,
                        syntax.Return(syntax.LitteralBoolean(True)),
                    ),
                    syntax.TaggedStmt(
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

    def get_program(self, condition: syntax.Expr) -> syntax.Program:
        return syntax.Program(
            [],  # No local variable
            [
                syntax.While(
                    condition,
                    syntax.TaggedStmt(
                        ON_TRUE_TAG,
                        syntax.Return(syntax.LitteralBoolean(True)),
                    ),
                ),
                syntax.TaggedStmt(
                    ON_FALSE_TAG, syntax.Return(syntax.LitteralBoolean(False))
                ),
            ],
        )


class Return(Context):
    TAG_CONTEXT = syntax.TagTypes.EXPRESSION

    def get_program(self, condition: syntax.Expr) -> syntax.Program:
        return syntax.Program(
            [],
            [
                syntax.Return(condition),
            ],
        )
