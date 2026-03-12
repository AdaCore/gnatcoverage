"""Expose operand kinds suitable for condition expressions."""

from __future__ import annotations

import abc
from collections.abc import Iterable, Sequence
from typing import ClassVar

import SCOV.expgen.syntax as syntax


class Operand:
    """
    A kind of operand for conditions: a variable, a comparison with an integer,
    a function call, etc.
    """

    def __init__(
        self,
        used_types: Iterable[syntax.Type],
        param_type: syntax.Type,
        actuals: dict[bool, syntax.Expr],
    ):
        # Set for all the types used by the operand kind. They will be used by
        # the serializer to output type declarations.
        self.used_types = set(used_types)

        # Type of the formal used in the operand. Must be in `used_types`.
        self.param_type = param_type

        # Two litterals that make the operand evaluate to False/True.
        self.actuals = actuals

    @abc.abstractmethod
    def get_operand(self, param: str) -> syntax.Expr:
        """Turn some parameter name into an abstract tree for the operand."""
        pass


class LanguageSpecific(Operand):
    """
    Language-specific operand, shortcuts the AST with text substitution.
    """

    LANGUAGE: ClassVar[str]
    """
    Name of the language this operand is specific to.
    """

    FORMAT: ClassVar[str]
    """
    String template of the operand expression. In order to let the engine
    insert the formal name used in the operand expression, the template should
    contain the "{formal_name}" placeholder.
    """

    USED_TYPES: ClassVar[Sequence[syntax.Type]]
    """
    Set of all the types used by this operand.
    """

    PARAM_TYPE: ClassVar[syntax.Type]
    """
    Type of the formal used in the operand.

    It must be one of the items returned in ``USED_TYPES
    """

    ACTUALS: ClassVar[dict[bool, str]]
    """
    Mapping for the two literals that make the operand evaluate to False/True.
    """

    def __init__(self) -> None:
        # Convert used types to syntax.XType nodes
        xtypes: list[syntax.Type] = []
        param_type: syntax.Type | None = None
        for type_ in self.USED_TYPES:
            xtypes.append(type_)

            # Pick the one that is PARAM_TYPE instead of generating PARAM_TYPE
            # itself, so that in the end, PARAM_TYPE still is in USED_TYPES.
            if type_ == self.PARAM_TYPE:
                param_type = type_

        assert param_type is not None, "param must be present in used types"

        # Convert actuals from strings to syntax.XLitteral nodes
        actuals_str = self.ACTUALS
        actuals: dict[bool, syntax.Expr] = {
            value: syntax.XLitteral(self.LANGUAGE, actuals_str[value])
            for value in (False, True)
        }

        super(LanguageSpecific, self).__init__(xtypes, param_type, actuals)

    def get_operand(self, param: str) -> syntax.Expr:
        return syntax.XOperand(self.LANGUAGE, self.FORMAT, param)


class Variable(Operand):
    """The operand is just the usage of a boolean argument."""

    def __init__(self) -> None:
        super(Variable, self).__init__(
            (syntax.BooleanType,),
            syntax.BooleanType,
            {
                False: syntax.LitteralBoolean(False),
                True: syntax.LitteralBoolean(True),
            },
        )

    def get_operand(self, param: str) -> syntax.Expr:
        return syntax.VariableUsage(param)


class IntegerComparison(Operand):
    """Operand that compares the parameter with a given integer."""

    # Provide a factory for the actuals for each relationnal operator.
    # Each factory takes the compared integer and returns (a false actual, a
    # true one).
    ACTUALS_MAKERS = {
        syntax.RelOp.GT: lambda value: (value - 1, value + 1),
        syntax.RelOp.GE: lambda value: (value - 1, value + 1),
        syntax.RelOp.LT: lambda value: (value + 1, value - 2),
        syntax.RelOp.LE: lambda value: (value + 1, value - 2),
        syntax.RelOp.EQ: lambda value: (value + 1, value),
        syntax.RelOp.NE: lambda value: (value, value + 1),
    }

    def __init__(self, operator: syntax.RelOp, value: int):
        actual_false, actual_true = self.ACTUALS_MAKERS[operator](value)
        super(IntegerComparison, self).__init__(
            (syntax.IntegerType,),
            syntax.IntegerType,
            {
                False: syntax.LitteralInteger(actual_false),
                True: syntax.LitteralInteger(actual_true),
            },
        )
        self.operator = operator
        self.value = syntax.LitteralInteger(value)

    def get_operand(self, param: str) -> syntax.Expr:
        return syntax.Comparison(
            self.operator, syntax.VariableUsage(param), self.value
        )
