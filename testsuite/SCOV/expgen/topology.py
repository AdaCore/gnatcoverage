"""
Expose the topology definition and instanciation engine.

A topology is used to specify the pattern of a decision: X and (not Y or Z),
for instance, is such a pattern, built from a boolean expression and
placeholders.

Once a topology is defined, it can be instanciated to a full decision
expression: to do so, placeholders are replaced by "operand" expressions.
"""

from __future__ import annotations

import dataclasses

import SCOV.expgen.context as context
import SCOV.expgen.syntax as syntax


@dataclasses.dataclass(frozen=True)
class OperandPlaceholder(syntax.Expr):
    """Placeholder to be put in expressions to specify a topology."""

    pass


class Topology:
    """
    Decision expression pattern that can be instanciated to fill the pattern
    with actual operands.
    """

    def __init__(self, expression: syntax.Expr):
        """Create a new topology using `expression` as its pattern."""
        self.expression = expression
        self.arity = self.get_arity(expression)

    def get_arity(self, expression: syntax.Expr) -> int:
        """Return the number of operand placeholders in the given
        expression."""
        if isinstance(expression, OperandPlaceholder):
            return 1
        else:
            result = 0
            for value in expression.iter_subexprs():
                if isinstance(expression, syntax.Expr):
                    result += self.get_arity(value)
            return result

    def instanciate(
        self,
        operands: list[syntax.Expr],
        formal_names: list[str],
        context: context.Context,
    ) -> syntax.Expr:
        """
        Instanciate the expression pattern tree replacing the placeholders with
        the given expressions, which are tagged with the given `formal_names`
        and with the given `context` (tags depend on the context).
        """

        def helper(expression: syntax.Expr, i: int) -> tuple[syntax.Expr, int]:
            """
            Likewise, helper to process sub-expressions in a recursive fashion.
            `i` is the first operand to replace. Return the instanciated
            expression and the index of the next operand to replace.
            """

            # If we find a placeholder, return the next operand after tagging
            # it.
            if isinstance(expression, OperandPlaceholder):
                result = syntax.TaggedExpr(
                    syntax.Tag("eval", formal_names[i], context.TAG_CONTEXT),
                    operands[i],
                )
                return (result, i + 1)

            # Otherwise, first instanciate sub-expressions of the given
            # expression pattern.
            sub_expressions = []
            for sub_expr in expression.iter_subexprs():
                instance, next_operand = helper(sub_expr, i)
                i = next_operand
                sub_expressions.append(instance)

            # And then return the instanciated expression
            ExprKind = type(expression)
            return ExprKind(*sub_expressions), i

        # The number of operands must naturally match the number of
        # placeholders in the expression pattern.
        assert len(operands) == self.arity

        expr, next_operand = helper(self.expression, 0)

        # All operands must have be replaced.
        assert next_operand == self.arity

        return expr

    def evaluate(self, operands: list[bool]) -> bool:
        """Evaluate the boolean expression using the given `operands`."""

        def helper(expression: syntax.Expr, i: int) -> tuple[bool, int]:
            """
            Likewise, helper to process sub-expressions in a recursive fashion.
            `i` is the first `operand` to use. Return the result of the given
            `expression` evaluation and the index of the next operand to use.
            """

            # If we find a placeholder, return the next operand.
            if isinstance(expression, OperandPlaceholder):
                return (operands[i], i + 1)

            # Otherwise, evaluate sub-expressions and return the result.
            if isinstance(expression, syntax.And):
                left, i = helper(expression.left, i)
                right, i = helper(expression.right, i)
                return (left and right, i)
            elif isinstance(expression, syntax.Or):
                left, i = helper(expression.left, i)
                right, i = helper(expression.right, i)
                return (left or right, i)
            elif isinstance(expression, syntax.Not):
                result, i = helper(expression.expr, i)
                return (not result, i)
            else:
                raise ValueError(
                    "Invalid topology node: {}".format(expression)
                )

        # The number of operands must naturally match the number of
        # placeholders in the expression pattern.
        assert len(operands) == self.arity

        result, next_operand = helper(self.expression, 0)

        # All operands must have be replaced.
        assert next_operand == self.arity

        return result

    def __str__(self) -> str:
        placeholders = list(reversed("ABCDEFGHIJ"))

        def helper(expr: syntax.Expr) -> str:
            if isinstance(expr, OperandPlaceholder):
                return placeholders.pop()
            elif isinstance(expr, syntax.And):
                return "({} and {})".format(
                    helper(expr.left), helper(expr.right)
                )
            elif isinstance(expr, syntax.Or):
                return "({} or {})".format(
                    helper(expr.left), helper(expr.right)
                )
            else:
                assert isinstance(expr, syntax.Not)
                return "not {}".format(helper(expr.expr))

        return helper(self.expression)
