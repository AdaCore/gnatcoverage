from __future__ import annotations

from collections.abc import Iterable
import contextlib
import types
from typing import IO

import SCOV.expgen.language as language
import SCOV.expgen.operand as operand
import SCOV.expgen.syntax as syntax
import SCOV.expgen.utils as utils


# Serialization of relational operators
REL_OP = {
    syntax.RelOp.GT: ">",
    syntax.RelOp.GE: ">=",
    syntax.RelOp.LT: ">",
    syntax.RelOp.LE: ">=",
    syntax.RelOp.EQ: "=",
    syntax.RelOp.NE: "/=",
}

# Serialization for builtin types
BUILTIN_TYPES = {
    syntax.Types.BOOLEAN: "Boolean",
    syntax.Types.INTEGER: "Integer",
}


def conv_name(name: str) -> str:
    """Convert a name to an Ada-style casing."""
    result = []
    up_next = True
    for c in name:
        if up_next:
            up_next = False
            result.append(c.upper())
            continue
        if c == "_":
            up_next = True
        result.append(c)
    return "".join(result)


class PackageGuard(contextlib.AbstractContextManager[None]):
    def __init__(self, language: Language, is_spec: bool, module_name: str):
        self.language = language
        self.is_spec = is_spec
        self.module_name = module_name

        # Package contents are indented.
        self.indent_guard = language.indent(language.INDENT)

    def __enter__(self) -> None:
        self.language.write(
            "package {}{} is".format(
                "" if self.is_spec else "body ", conv_name(self.module_name)
            )
        )
        self.language.newline()
        self.indent_guard.__enter__()

    def __exit__(
        self,
        exctype: type[BaseException] | None,
        value: BaseException | None,
        traceback: types.TracebackType | None,
    ) -> None:
        self.indent_guard.__exit__(exctype, value, traceback)
        self.language.write("end {};".format(conv_name(self.module_name)))
        self.language.newline()


class Language(language.Language):

    NAME = "Ada"
    INDENT = 3

    #
    # Filename generators
    #

    def get_specification_filename(self, module_name: str) -> str:
        return "{}.ads".format(module_name)

    def get_implementation_filename(self, module_name: str) -> str:
        return "{}.adb".format(module_name)

    #
    # Serialization entry points
    #

    def serialize_run_module_implementation(
        self,
        stream: IO[str],
        operand_kinds: list[operand.Operand],
        truth_vectors: set[tuple[bool, ...]],
    ) -> None:
        self.set_stream(stream)

        self.add_use(self.SUPPORT_MODULE)
        self.add_use(self.TYPES_MODULE)
        self.add_use(self.COMPUTING_MODULE)
        self.newline()

        with PackageGuard(self, False, self.RUN_MODULE):
            # For each truth vector, generate a run procedure.
            for i, truth_vector in enumerate(truth_vectors):
                if i > 0:
                    self.newline()

                procedure_name = self.get_run_procedure_name(truth_vector)
                self.add_subprogram_signature(
                    procedure_name, None, [], [], False
                )
                self.write("begin")
                self.newline()
                with self.indent(self.INDENT):
                    call_to_run = syntax.Call(
                        syntax.VariableUsage(self.ENTRY_POINT_NAME),
                        [
                            op_kind.actuals[op_truth]
                            for op_kind, op_truth in zip(
                                operand_kinds, truth_vector
                            )
                        ],
                    )
                    self.handle_expr(
                        syntax.Call(
                            syntax.VariableUsage(self.ASSERT_PROC_NAME),
                            [
                                syntax.Comparison(
                                    syntax.RelOp.EQ,
                                    call_to_run,
                                    syntax.LitteralBoolean(truth_vector[-1]),
                                )
                            ],
                        )
                    )
                    self.write(";")
                    self.newline()
                self.write("end {};".format(conv_name(procedure_name)))
                self.newline()

    # This one is specific to the Ada language, since this is the main
    # testsuite language (the one used by test drivers.
    def serialize_run_module_interface(
        self,
        stream: IO[str],
        target_language: language.Language,
        truth_vectors: set[tuple[bool, ...]],
    ) -> None:
        self.set_stream(stream)

        self.add_use(self.SUPPORT_MODULE)
        self.newline()

        with PackageGuard(self, True, self.RUN_MODULE):
            # Generate a run procedure binding for each run procedure, so for
            # each truth vector.
            for i, truth_vector in enumerate(truth_vectors):
                if i > 0:
                    self.newline()

                procedure_name = self.get_run_procedure_name(truth_vector)
                self.add_subprogram_signature(
                    procedure_name, None, [], [], True
                )
                # If the target language is not Ada, use the C
                # calling/mangling/whatever... convention.
                if target_language.NAME != self.NAME:
                    self.write(
                        'Pragma Import(C, {}, "{}");'.format(
                            conv_name(procedure_name), procedure_name
                        )
                    )
                    self.newline()

    def serialize_specification_types(
        self,
        stream: IO[str],
        types: Iterable[syntax.Type],
    ) -> None:
        self.set_stream(stream)
        with PackageGuard(self, True, self.TYPES_MODULE):
            for type_decl in types:
                if not isinstance(type_decl, syntax.BuiltinType):
                    self.handle_type(type_decl, declaration=True)

    def serialize_specification_program(
        self,
        stream: IO[str],
        formal_names: list[str],
        formal_types: list[syntax.Type],
    ) -> None:
        self.set_stream(stream)

        self.add_use(self.TYPES_MODULE)
        self.newline()

        with PackageGuard(self, True, self.COMPUTING_MODULE):
            self.add_subprogram_signature(
                self.ENTRY_POINT_NAME,
                syntax.BooleanType,
                formal_names,
                formal_types,
                True,
            )

    def serialize_implementation(
        self,
        stream: IO[str],
        program: syntax.Program,
        formal_names: list[str],
        formal_types: list[syntax.Type],
        one_operand_per_line: bool,
    ) -> None:
        self.set_stream(stream)

        self.one_operand_per_line = one_operand_per_line

        self.add_use(self.SUPPORT_MODULE)

        with PackageGuard(self, False, self.COMPUTING_MODULE):
            self.add_subprogram_signature(
                self.ENTRY_POINT_NAME,
                syntax.BooleanType,
                formal_names,
                formal_types,
                False,
            )
            self.handle_program(program)

    #
    # Various helpers
    #

    def add_use(self, module_name: str) -> None:
        self.write(
            "with {name}; use {name};".format(name=conv_name(module_name))
        )
        self.newline()

    def add_subprogram_signature(
        self,
        name: str,
        return_type: syntax.Type | None,
        formal_names: list[str],
        formal_types: list[syntax.Type],
        declaration: bool,
    ) -> None:
        """Add a subprogram signature to the output.

        If `return_type` is None, the subprogram is considered as a procedure.
        It is a function otherwise.
        """
        # Add the type of subprogram and its name.
        self.write(
            "{} {}".format(
                "function" if return_type else "procedure",
                conv_name(name),
            )
        )

        # Add the list of formals.
        if len(formal_names) > 0:
            self.write(" (")
            for i, (name, type_) in enumerate(zip(formal_names, formal_types)):
                if i > 0:
                    self.write("; ")
                self.write("{} : ".format(conv_name(name)))
                self.handle_type(type_)
            self.write(")")

        # Add the return type, if any
        if return_type:
            self.write(" return ")
            self.handle_type(return_type)

        self.write(";" if declaration else " is")
        self.newline()

    def handle_parent(self, expr: syntax.Expr) -> None:
        self.write("(")
        with self.indent():
            self.handle_expr(expr)
        self.write(")")

    def helper_binop(
        self,
        op: str,
        left: syntax.Expr,
        right: syntax.Expr,
    ) -> None:
        self.handle_composite_expr(left)
        if self.one_operand_per_line and utils.contains_tag(left):
            self.newline()
        else:
            self.write(" ")
        self.write("{} ".format(op))
        self.handle_composite_expr(right)

    def format_comment(self, string: str) -> str:
        return "-- {}".format(string)

    def handle_program(
        self,
        program: syntax.Program,
        declaration: bool = False,
    ) -> None:
        with self.indent(self.INDENT):
            for name, type_ in program.local_vars:
                self.write("{} : ".format(conv_name(name)))
                self.handle_type(type_)
                self.write(";")
                self.newline()

        self.write("begin")
        self.newline()
        with self.indent(self.INDENT):
            for stmt in program.statements:
                self.handle_stmt(stmt)
        self.write("end;")
        self.newline()

    #
    # Serialization for types
    #

    def handle_builtin_type(
        self,
        builtin_type: syntax.BuiltinType,
        declaration: bool = False,
    ) -> None:
        if declaration:
            raise ValueError(
                "Cannot output a type declaration for a builtin type"
            )
        else:
            self.write(BUILTIN_TYPES[builtin_type.ref])

    def handle_record_type(
        self,
        record_type: syntax.RecordType,
        declaration: bool = False,
    ) -> None:
        if declaration:
            self.write("type {} is record".format(conv_name(record_type.name)))
            self.newline()
            with self.indent(self.INDENT):
                for member in record_type.members:
                    self.handle_member_decl(member)
            self.write("end record;")
            self.newline()
        else:
            self.write(conv_name(record_type.name))

    def handle_member_decl(self, member_decl: syntax.MemberDecl) -> None:
        self.write("{} : ".format(conv_name(member_decl.name)))
        self.handle_type(member_decl.type_)
        self.write(";")
        self.newline()

    #
    # Serialization for expressions
    #

    def handle_variable_usage(self, var: syntax.VariableUsage) -> None:
        self.write("{}".format(conv_name(var.name)))

    def handle_litteral_integer(self, integer: syntax.LitteralInteger) -> None:
        self.write("{}".format(integer.value))

    def handle_litteral_boolean(self, boolean: syntax.LitteralBoolean) -> None:
        # Python and Ada happen to share the same syntax for  litteral
        # booleans.
        self.write(str(boolean.value))

    def handle_litteral_record(self, record: syntax.LitteralRecord) -> None:
        self.write("(")
        assert isinstance(record.type_, syntax.RecordType)
        for i, (member, decl) in enumerate(
            zip(record.members, record.type_.members)
        ):
            if i > 0:
                self.write(", ")
            self.write("{} => ".format(conv_name(decl.name)))
            self.handle_expr(member)
        self.write(")")

    def handle_comparison(self, comp: syntax.Comparison) -> None:
        self.helper_binop(REL_OP[comp.operator], comp.left, comp.right)

    def handle_call(self, expr: syntax.Call) -> None:
        self.handle_composite_expr(expr.function)
        if len(expr.arguments) > 0:
            self.write("(")
            with self.indent():
                for i, arg in enumerate(expr.arguments):
                    self.handle_expr(arg)
                    if i + 1 < len(expr.arguments):
                        self.write(",")
                        self.newline()
            self.write(")")

    #
    # Serialization for topology expressions
    #

    def handle_and_expr(self, expr: syntax.And) -> None:
        self.helper_binop("and then", expr.left, expr.right)

    def handle_or_expr(self, expr: syntax.Or) -> None:
        self.helper_binop("or else", expr.left, expr.right)

    def handle_not_expr(self, expr: syntax.Not) -> None:
        self.write("not ")
        self.handle_composite_expr(expr.expr)

    def handle_composite_expr(self, expr: syntax.Expr) -> None:
        is_composite = isinstance(
            expr,
            (
                syntax.Comparison,
                syntax.And,
                syntax.Or,
            ),
        )

        if is_composite:
            self.write("(")
        with self.indent():
            self.handle_expr(expr)
        if is_composite:
            self.write(")")

    #
    # Serialization for statements
    #

    def handle_if_stmt(self, stmt: syntax.If) -> None:
        self.write("if ")
        with self.indent():
            self.handle_expr(stmt.condition)
        self.write(" then")
        self.newline()

        with self.indent(self.INDENT):
            self.handle_stmt(stmt.true_stmt)

        self.write("else")
        self.newline()
        with self.indent(self.INDENT):
            self.handle_stmt(stmt.false_stmt)
        self.write("end if;")
        self.newline()

    def handle_while_stmt(self, stmt: syntax.While) -> None:
        self.write("while ")
        with self.indent():
            self.handle_expr(stmt.condition)
        self.write(" loop")
        self.newline()

        with self.indent(self.INDENT):
            self.handle_stmt(stmt.stmt)

        self.write("end loop;")
        self.newline()

    def handle_return_stmt(self, stmt: syntax.Return) -> None:
        self.write("return ")
        with self.indent():
            self.handle_expr(stmt.expr)
        self.write(";")
        self.newline()

    def handle_assign_stmt(self, stmt: syntax.Assign) -> None:
        self.handle_expr(stmt.variable)
        self.write(" := ")
        self.handle_expr(stmt.expr)
        self.write(";")
        self.newline()
