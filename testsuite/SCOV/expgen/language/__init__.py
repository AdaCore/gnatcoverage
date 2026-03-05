"""
Expose the base class for Language serializers and some formatting helpers.
"""

from __future__ import annotations

import abc
from collections.abc import Iterable
import contextlib
import dataclasses
import io
import types
from typing import ClassVar, IO

import SCOV.expgen.syntax as syntax
import SCOV.expgen.context as context
import SCOV.expgen.operand as operand
import SCOV.expgen.utils as utils


class Language:
    """
    Base class for language serializers. To add a new language, subclass it.

    Subclasses have to define the `NAME` attribute to the name of the language,
    and they must define handlers for AST nodes (`handle_NODE_NAME` methods)
    that serialize those nodes to the formatter. Note that language-specific
    nodes (ast.X*) already have ready-to-use implementations here.
    """

    NAME: ClassVar[str]

    INDENT = 2

    SUPPORT_MODULE = "support"
    # The "types" module is used to define types used by operands.
    TYPES_MODULE = "types"
    # The "run" module contains all "run_*" procedures called by test drivers.
    RUN_MODULE = "run"
    # These "run_*" functions call the "compute" function in the "computing"
    # module.
    COMPUTING_MODULE = "computing"
    ENTRY_POINT_NAME = "compute"
    ASSERT_PROC_NAME = "assert"

    # Used to format the name of the test procedures.
    BOOL_TO_CHAR = {
        False: "F",
        True: "T",
    }

    def __init__(self) -> None:
        self.one_operand_per_line = True

    #
    # Filename generators
    #

    @abc.abstractmethod
    def get_specification_filename(self, module_name: str) -> str:
        pass

    @abc.abstractmethod
    def get_implementation_filename(self, module_name: str) -> str:
        pass

    #
    # Serialization entry points
    #

    @abc.abstractmethod
    def serialize_run_module_implementation(
        self,
        stream: IO[str],
        operand_kinds: list[operand.Operand],
        truth_vectors: set[tuple[bool, ...]],
    ) -> None:
        """
        Output the implementation of the run module, i.e. all "run_*"
        procedure. These procedures run the "compute" function with actuals
        that evaluate to the value specified in `truth_vector` for the given
        `operand_kinds`.

        `truth_vectors` is a list of lists of boolean: one list per "run_*"
        procedure, that contain one boolean per argument for the "compute"
        function, plus one for the expected result.
        """
        pass

    @abc.abstractmethod
    def serialize_specification_types(
        self,
        stream: IO[str],
        types: Iterable[syntax.Type],
    ) -> None:
        """
        Output a specification source that contains declarations for the given
        `types`. The name of the module for these specification is
        `TYPES_MODULE`.
        """
        pass

    @abc.abstractmethod
    @abc.abstractmethod
    def serialize_specification_program(
        self,
        stream: IO[str],
        formal_names: list[str],
        formal_types: list[syntax.Type],
    ) -> None:
        """
        Output a specification source that contain a subprogram declaration for
        the "compute" function, given its formal names and types. The name of
        the module for these specification is `COMPUTING_MODULE`.
        """
        pass

    @abc.abstractmethod
    def serialize_implementation(
        self,
        stream: IO[str],
        program: syntax.Program,
        formal_names: list[str],
        formal_types: list[syntax.Type],
        one_operand_per_line: bool,
    ) -> None:
        """
        Output an implementation source that contain the "compute" function,
        given its formal names and types. The name of the module for these
        specification is `COMPUTING_MODULE`.
        """
        pass

    def format_expr(self, expr: syntax.Expr) -> tuple[str, syntax.Tag | None]:
        """
        Instead of serializing the given `tree` into the output stream, return
        the serialization as a string plus the resulting tag, if any.
        """
        # Save the current formatter and replace it with a buffer one.
        output_buffer = io.StringIO()
        formatter = self.formatter
        self.set_formatter(formatter.sub(output_buffer))

        # Let handlers serialize the `tree` into the buffer.
        self.handle_expr(expr)
        # Save the tag that remains in the buffer formatter, if any.
        remaining_tag = self.formatter.line_tag

        # Restore the old formatter.
        self.set_formatter(formatter)

        # And return the buffered serialization.
        return (output_buffer.getvalue(), remaining_tag)

    @abc.abstractmethod
    def format_comment(self, string: str) -> str:
        """
        Return a line comment for the supported language that contains the
        given `string`.
        """
        pass

    @abc.abstractmethod
    def handle_program(
        self,
        program: syntax.Program,
        declaration: bool = False,
    ) -> None:
        pass

    @abc.abstractmethod
    def handle_parent(self, node: syntax.Expr) -> None:
        """Handle the given `node`, but add parenthesis around it."""
        pass

    def handle_stmt(self, stmt: syntax.Stmt) -> None:
        match stmt:
            case syntax.If():
                self.handle_if_stmt(stmt)
            case syntax.While():
                self.handle_while_stmt(stmt)
            case syntax.Return():
                self.handle_return_stmt(stmt)
            case syntax.Assign():
                self.handle_assign_stmt(stmt)
            case syntax.TaggedStmt():
                self.handle_tagged_node(stmt)
            case _:
                raise AssertionError(f"unknown stmt: {stmt!r}")

    @abc.abstractmethod
    def handle_if_stmt(self, stmt: syntax.If) -> None:
        pass

    @abc.abstractmethod
    def handle_while_stmt(self, stmt: syntax.While) -> None:
        pass

    @abc.abstractmethod
    def handle_return_stmt(self, stmt: syntax.Return) -> None:
        pass

    @abc.abstractmethod
    def handle_assign_stmt(self, stmt: syntax.Assign) -> None:
        pass

    def handle_type(
        self,
        type_: syntax.Type,
        declaration: bool = False,
    ) -> None:
        match type_:
            case syntax.BuiltinType():
                self.handle_builtin_type(type_, declaration)
            case syntax.RecordType():
                self.handle_record_type(type_, declaration)
            case _:
                raise AssertionError(f"unknown type: {type_!r}")

    @abc.abstractmethod
    def handle_builtin_type(
        self,
        type_: syntax.BuiltinType,
        declaration: bool,
    ) -> None:
        pass

    @abc.abstractmethod
    def handle_record_type(
        self,
        type_: syntax.RecordType,
        declaration: bool,
    ) -> None:
        pass

    def handle_expr(self, expr: syntax.Expr) -> None:
        match expr:
            case syntax.VariableUsage():
                self.handle_variable_usage(expr)
            case syntax.LitteralInteger():
                self.handle_litteral_integer(expr)
            case syntax.LitteralBoolean():
                self.handle_litteral_boolean(expr)
            case syntax.LitteralRecord():
                self.handle_litteral_record(expr)
            case syntax.Comparison():
                self.handle_comparison(expr)
            case syntax.Call():
                self.handle_call(expr)
            case syntax.And():
                self.handle_and_expr(expr)
            case syntax.Or():
                self.handle_or_expr(expr)
            case syntax.Not():
                self.handle_not_expr(expr)
            case syntax.TaggedExpr():
                self.handle_tagged_node(expr)
            case _:
                raise AssertionError(f"unknown expr: {expr!r}")

    @abc.abstractmethod
    def handle_variable_usage(self, expr: syntax.VariableUsage) -> None:
        pass

    @abc.abstractmethod
    def handle_litteral_integer(self, expr: syntax.LitteralInteger) -> None:
        pass

    @abc.abstractmethod
    def handle_litteral_boolean(self, expr: syntax.LitteralBoolean) -> None:
        pass

    @abc.abstractmethod
    def handle_litteral_record(self, expr: syntax.LitteralRecord) -> None:
        pass

    @abc.abstractmethod
    def handle_comparison(self, expr: syntax.Comparison) -> None:
        pass

    @abc.abstractmethod
    def handle_call(self, expr: syntax.Call) -> None:
        pass

    @abc.abstractmethod
    def handle_and_expr(self, expr: syntax.And) -> None:
        pass

    @abc.abstractmethod
    def handle_or_expr(self, expr: syntax.Or) -> None:
        pass

    @abc.abstractmethod
    def handle_not_expr(self, expr: syntax.Not) -> None:
        pass

    #
    # Various helpers
    #

    def set_stream(self, stream: IO[str]) -> None:
        """
        Create a new formatter to wrap the given `stream` and use it.
        """
        self.set_formatter(Formatter(self, stream))

    def set_formatter(self, formatter: Formatter) -> None:
        """Make the current Language instance use the given `formatter`."""
        self.formatter = formatter

        # Shortcuts to very used formatter methods.
        self.indent = formatter.indent
        self.write = formatter.write
        self.newline = formatter.newline
        self.add_tag = formatter.add_tag

    def check_language(
        self,
        xnode: (
            syntax.XType | syntax.XLitteral | syntax.XOperand | syntax.XContext
        ),
    ) -> None:
        """
        Assert that the given `xnode` handle the language that is handled by
        this instance.
        """
        assert xnode.language == self.NAME, (
            "{} construct is specific to {} "
            "but is used with language {}".format(
                type(xnode).__name__, xnode.language, self.NAME
            )
        )

    def handle_tagged_node(self, tagged_node: syntax.TaggedNode) -> None:
        """
        Add the tag of the given node and handle the nested node. Parenthesis
        are added if needed.
        """
        self.add_tag(tagged_node.tag)
        # If the tagged node is an expression, add parenthesis in order to be
        # able to refer to this sub-expression from coverage expectations.
        if isinstance(tagged_node.node, syntax.Expr):
            self.handle_parent(tagged_node.node)
        else:
            self.handle_stmt(tagged_node.node)

    def get_run_procedure_name(self, truth_vector: tuple[bool, ...]) -> str:
        return "run_{}_{}".format(
            "".join(
                self.BOOL_TO_CHAR[b] for b in truth_vector[:-1]
            ),  # Actuals
            self.BOOL_TO_CHAR[truth_vector[-1]],  # Result
        )

    #
    # Specific-language nodes (ast.X*) handlers
    #

    # Each of these check the language of the given node before using it...

    def handle_language_specific_type(
        self,
        xtype: syntax.XType,
        declaration: bool = False,
    ) -> None:
        self.check_language(xtype)
        if declaration:
            for line in xtype.declaration:
                self.write(line)
                self.newline()
        else:
            self.write(xtype.usage)

    def handle_language_specific_litteral(
        self,
        xlitteral: syntax.XLitteral,
    ) -> None:
        self.check_language(xlitteral)
        self.write(xlitteral.format_str)

    def handle_language_specific_operand(
        self,
        xoperand: syntax.XOperand,
    ) -> None:
        self.check_language(xoperand)
        # Languages can process identifiers in a specific way.
        formal_name, remaining_tag = self.format_expr(
            syntax.VariableUsage(xoperand.formal_name)
        )
        if remaining_tag:
            self.add_tag(remaining_tag)
        self.write(xoperand.format_str.format(formal_name=formal_name))

    def handle_language_specific_context(
        self,
        xcontext: syntax.XContext,
    ) -> None:
        self.check_language(xcontext)
        for line in xcontext.format_str:
            try:
                index = line.index("{decision_expr}")
            except ValueError:
                self.write(line)
            else:
                # Write the text before the decision expression placeholder
                # formatting so that indentation is correctly set during the
                # formatting.
                first_half, second_half = line[:index], line[index:]
                self.write(first_half)

                with self.indent(self.INDENT):
                    decision_expr, remaining_tag = self.format_expr(
                        xcontext.decision_expr
                    )
                    second_half = second_half.format(
                        decision_expr=decision_expr
                    )
                    if remaining_tag:
                        self.add_tag(remaining_tag)
                    self.write(second_half)
            self.newline()

    def _filter_nodes[
        T
    ](self, specific_class: type, node_kinds: list[T],) -> list[T]:
        """
        Return the subset of `node_kinds` that can be used with this language,
        `specific_class` being the kind of nodes that can be specific to
        a language.
        """
        return list(
            filter(
                lambda node_kind: (
                    not isinstance(node_kind, specific_class)
                    or (
                        isinstance(node_kind, syntax.LanguageSpecific)
                        and node_kind.language == self.NAME
                    )
                ),
                node_kinds,
            )
        )

    def filter_contexts(
        self,
        contexts: list[context.Context],
    ) -> list[context.Context]:
        """
        Return the subset of `contexts` that can be used with this language.
        """
        return self._filter_nodes(context.LanguageSpecific, contexts)

    def filter_operand_kinds(
        self,
        operand_kinds: list[operand.Operand],
    ) -> list[operand.Operand]:
        """
        Return the subset of `operand_kinds` that can be used with this
        language.
        """
        return self._filter_nodes(operand.LanguageSpecific, operand_kinds)

    def filter_types(self, types: list[syntax.Type]) -> list[syntax.Type]:
        """
        Return the subset of `types` that can be used with this language.
        """
        return self._filter_nodes(syntax.XType, types)


#
# Formatting helper classes
#


class IndentationGuard(contextlib.AbstractContextManager[None]):
    """
    Increment the indentation level on entry and decrement it when leaving.
    """

    def __init__(self, formatter: Formatter, addend: int):
        self.formatter = formatter
        self.addend = addend

    def __enter__(self) -> None:
        self.formatter.push_indent(self.addend)

    def __exit__(
        self,
        exctype: type[BaseException] | None,
        value: BaseException | None,
        traceback: types.TracebackType | None,
    ) -> None:
        self.formatter.pop_indent()


class Formatter:
    """Output stream wrapper that takes care of indentation, line tags, etc."""

    def __init__(self, language: Language, stream: IO[str]):
        self.language = language
        self.stream = stream

        # Current line number of characters
        self.current_column = 0
        # Identation levels stack. Topmost (self.indent_stack[-1]) will be the
        # indentation level of the next line.
        self.indent_stack = [0]

        # Tag for the current line. Flushed on newline.
        self.line_tag: syntax.Tag | None = None

    def sub(self, stream: IO[str]) -> Formatter:
        """
        Return a new formatter that have a "sub-state" (same indentation state,
        but no duplicated tag), with another output stream.
        """
        result = Formatter(self.language, stream)
        result.current_column = self.current_column
        result.indent_stack = list(self.indent_stack)
        result.line_tag = None
        return result

    def write(self, string: str) -> None:
        """Write some `string` into the wrapped stream.

        Take care of column handling. The given `string` must not contain a new
        line character.
        """
        if self.current_column == 0:
            self.current_column = self.indent_stack[-1]
            self.stream.write(" " * self.indent_stack[-1])
        self.stream.write(string)
        self.current_column += len(string)

    def push_indent(self, addend: int = 0) -> None:
        """Add an identation level.

        The new level is based on the current column plus `addend` columns.
        """
        old_level = self.current_column or self.indent_stack[-1]
        self.indent_stack.append(old_level + addend)

    def pop_indent(self) -> None:
        """Pop the newest indentation level."""
        self.indent_stack.pop()

    def indent(self, addend: int = 0) -> IndentationGuard:
        """Create and return an identation guard."""
        return IndentationGuard(self, addend)

    def newline(self) -> None:
        """Flush any tag and insert a new line character."""
        self.flush_tags()
        self.stream.write("\n")
        self.current_column = 0

    def add_tag(self, tag: syntax.Tag) -> None:
        """Add a `tag` to the current line."""
        utils.check_tag(tag)
        if self.line_tag:
            if tag.name != self.line_tag.name:
                raise ValueError(
                    "Trying to insert a `{}` tag on a line where there is a "
                    "`{}` tag".format(tag.name, self.line_tag.name)
                )
            if tag.context != self.line_tag.context:
                raise ValueError(
                    "Trying to insert a `{}` tag on a line where there is a "
                    "`{}` tag".format(tag.context, self.line_tag.context)
                )
            tag = dataclasses.replace(tag, operand="all")
        self.line_tag = tag

    def flush_tags(self) -> None:
        """
        Flush any tag. Should be called only through the `newline` method.
        """
        if self.line_tag is not None:
            comment = self.language.format_comment(
                utils.format_tag(self.line_tag)
            )
            self.stream.write(" {}".format(comment))
            self.line_tag = None
