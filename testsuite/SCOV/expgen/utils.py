"""Various helpers used through the whole package."""

from __future__ import annotations

import dataclasses

import SCOV.expgen.syntax as syntax


def check_tag(tag: syntax.Tag) -> None:
    """Assert whether the given `tag` is valid, or not."""
    if tag.name == "eval":
        assert tag.context in (
            syntax.TagTypes.DECISION,
            syntax.TagTypes.EXPRESSION,
        )
        assert tag.operand is not None
    else:
        assert tag.operand is None
        assert tag.context is None


def format_tag(tag: syntax.Tag) -> str:
    """Serialize the given `tag`."""
    if tag.name == "eval":
        return "# {}-{} {}".format(tag.name, tag.operand, tag.context)
    else:
        return "# {}".format(tag.name)


def contains_tag(node: object) -> bool:
    """Return if the given `node` tree contains a tagged node."""
    if isinstance(node, (str, bool, int)):
        return False

    if isinstance(node, syntax.TaggedNode):
        return True

    if dataclasses.is_dataclass(node) and not isinstance(node, type):
        return any(
            contains_tag(subnode) for subnode in dataclasses.astuple(node)
        )

    return False


def is_expr(node: object) -> bool:
    """Return whether `node` is an expression."""
    return isinstance(
        node,
        (
            syntax.VariableUsage,
            syntax.LitteralInteger,
            syntax.LitteralBoolean,
            syntax.LitteralRecord,
            syntax.Comparison,
            syntax.Call,
            syntax.XLitteral,
            syntax.XOperand,
            syntax.And,
            syntax.Or,
            syntax.Not,
        ),
    )


def is_topology_equal(topo1: syntax.Expr, topo2: syntax.Expr) -> bool:
    """Return whether two topologies are equal."""
    if type(topo1) is not type(topo2):
        return False
    elif isinstance(topo1, (syntax.And, syntax.Not, syntax.Or)):
        return all(
            is_topology_equal(sub_topo1, sub_topo2)
            for sub_topo1, sub_topo2 in zip(
                dataclasses.astuple(topo1),
                dataclasses.astuple(topo2),
            )
        )
    else:
        # typo1 and typo2 are placeholders
        return True
