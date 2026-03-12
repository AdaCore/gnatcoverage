"""
Expose various construct that can be used to build abstract trees. Those trees
can then be used to generate programs.

The idea is not to reimplement a full AST for both C and Ada, but rather focus
on an interesting common set of constructs for tests. When too specific
constructs are needed, just use the X* node kinds: XType, XOperand, etc.
"""

from __future__ import annotations

from collections.abc import Iterator, Sequence
import dataclasses
import enum
from typing import Generic, TypeVar


class RelOp(enum.StrEnum):
    """Available relational operators."""

    GT = ">"
    GE = ">="
    LT = "<"
    LE = "<="
    EQ = "=="
    NE = "!="


class Types(enum.StrEnum):
    """Available basic types for local variable declarations."""

    BOOLEAN = "boolean"
    INTEGER = "integer"


class TagTypes(enum.StrEnum):
    """Available decision context types for tags."""

    DECISION = ":d:"
    EXPRESSION = ":e:"


@dataclasses.dataclass(frozen=True)
class Program:
    local_vars: list[tuple[str, Type]]
    statements: list[Stmt]


# Types: used for both declarations and usages
@dataclasses.dataclass(frozen=True)
class Type:
    pass


@dataclasses.dataclass(frozen=True)
class BuiltinType(Type):
    ref: Types

    @property
    def name(self) -> str:
        return self.ref.name


@dataclasses.dataclass(frozen=True)
class RecordType(Type):
    name: str
    members: list[MemberDecl]


@dataclasses.dataclass(frozen=True)
class MemberDecl:
    type_: Type
    name: str


# ... and some regularly used ones.  Note that builtin types must not be
# declared
BooleanType = BuiltinType(Types.BOOLEAN)
IntegerType = BuiltinType(Types.INTEGER)


# Expressions: when adding nodes to these or to topology ones, do not forget to
# update the `utils.is_expr` function.
@dataclasses.dataclass(frozen=True)
class Expr:

    def iter_subexprs(self) -> Iterator[Expr]:
        for field in dataclasses.fields(self):
            sub_expr = getattr(self, field.name)
            if isinstance(sub_expr, Expr):
                yield sub_expr


@dataclasses.dataclass(frozen=True)
class VariableUsage(Expr):
    name: str


@dataclasses.dataclass(frozen=True)
class LitteralInteger(Expr):
    value: int


@dataclasses.dataclass(frozen=True)
class LitteralBoolean(Expr):
    value: bool


@dataclasses.dataclass(frozen=True)
class LitteralRecord(Expr):
    type_: Type
    members: list[Expr]


@dataclasses.dataclass(frozen=True)
class Comparison(Expr):
    operator: RelOp
    left: Expr
    right: Expr


@dataclasses.dataclass(frozen=True)
class Call(Expr):
    function: VariableUsage
    arguments: list[Expr]


# Topology expressions
@dataclasses.dataclass(frozen=True)
class And(Expr):
    left: Expr
    right: Expr


@dataclasses.dataclass(frozen=True)
class Or(Expr):
    left: Expr
    right: Expr


@dataclasses.dataclass(frozen=True)
class Not(Expr):
    expr: Expr


# Statements
@dataclasses.dataclass(frozen=True)
class Stmt:
    pass


@dataclasses.dataclass(frozen=True)
class If(Stmt):
    condition: Expr
    true_stmt: Stmt
    false_stmt: Stmt


@dataclasses.dataclass(frozen=True)
class While(Stmt):
    condition: Expr
    stmt: Stmt


@dataclasses.dataclass(frozen=True)
class Return(Stmt):
    expr: Expr


@dataclasses.dataclass(frozen=True)
class Assign(Stmt):
    variable: VariableUsage
    expr: Expr


# Language-specific constructs:


@dataclasses.dataclass(frozen=True)
class LanguageSpecific:
    language: str


# - Type
@dataclasses.dataclass(frozen=True)
class XType(LanguageSpecific, Type):
    language: str
    declaration: Sequence[str]
    usage: str


# - Litteral (for actuals, mostly)
@dataclasses.dataclass(frozen=True)
class XLitteral(LanguageSpecific, Expr):
    language: str
    format_str: str


# - Operand
@dataclasses.dataclass(frozen=True)
class XOperand(LanguageSpecific, Expr):
    language: str
    format_str: str
    formal_name: str


# - Context
@dataclasses.dataclass(frozen=True)
class XContext(LanguageSpecific, Stmt):
    language: str
    format_str: list[str]
    decision_expr: Expr


# Tag
#
# Used to tag lines, so that test drivers can reference the correct lines for
# expectations.
#
# * Custom tags may only define the `name` attribute, leaving `operand` and
#   `context` as None.
#
# * If name is `eval`, `operand` must be the formal name that is used in the
#   tagged operand, or `all` if all operands are on the same line. In both
#   cases, `context` must be `TagTypes.(DECISION or EXPRESSION)` depending on
#   the context of the tagged decision.
@dataclasses.dataclass(frozen=True)
class Tag:
    name: str
    operand: object | None
    context: object | None


NodeType = TypeVar("NodeType")


@dataclasses.dataclass(frozen=True)
class TaggedNode(Generic[NodeType]):
    tag: Tag
    node: NodeType


@dataclasses.dataclass(frozen=True)
class TaggedStmt(Stmt, TaggedNode[Stmt]):
    pass


@dataclasses.dataclass(frozen=True)
class TaggedExpr(Expr, TaggedNode[Expr]):
    pass
