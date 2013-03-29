# -*- coding: utf-8 -*-

import itertools

import SCOV.expgen.ast as ast
import SCOV.expgen.utils as utils
import SCOV.expgen.language as language


# Serialization of relational operators
REL_OP = {
    ast.RelOp.GT: '>',
    ast.RelOp.GE: '>=',
    ast.RelOp.LT: '>',
    ast.RelOp.LE: '>=',
    ast.RelOp.EQ: '=',
    ast.RelOp.NE: '/=',
}

# Serialization for builtin types
BUILTIN_TYPES = {
    ast.Types.BOOLEAN: 'Boolean',
    ast.Types.INTEGER: 'Integer',
}


def conv_name(name):
    '''
    Convert a name to an Ada-style casing.
    '''
    result = []
    up_next = True
    for c in name:
        if up_next:
            up_next = False
            result.append(c.upper())
            continue
        if c == '_':
            up_next = True
        result.append(c)
    return ''.join(result)


class PackageGuard(object):
    def __init__(self, language, is_spec, module_name):
        self.language = language
        self.is_spec = is_spec
        self.module_name = module_name

        # Package contents are indented.
        self.indent_guard = language.indent(language.INDENT)

    def __enter__(self):
        self.language.write('package {}{} is'.format(
            '' if self.is_spec else 'body ',
            conv_name(self.module_name)
        ))
        self.language.newline()
        self.indent_guard.__enter__()

    def __exit__(self, type, value, traceback):
        self.indent_guard.__exit__(type, value, traceback)
        self.language.write(
            'end {};'.format(conv_name(self.module_name))
        )
        self.language.newline()


class Language(language.Language):

    NAME = 'Ada'
    INDENT = 3


    #
    # Filename generators
    #

    def get_specification_filename(self, module_name):
        return '{}.ads'.format(module_name)

    def get_implementation_filename(self, module_name):
        return '{}.adb'.format(module_name)


    #
    # Serialization entry points
    #

    def serialize_run_module_implementation(
        self, stream, operand_kinds, truth_vectors
    ):
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
                self.write('begin'); self.newline()
                with self.indent(self.INDENT):
                    call_to_run = ast.Call(
                        ast.VariableUsage(self.ENTRY_POINT_NAME),
                        [
                            op_kind.actuals[op_truth]
                            for op_kind, op_truth
                            in itertools.izip(operand_kinds, truth_vector)
                        ]
                    )
                    self.handle(ast.Call(
                        ast.VariableUsage(self.ASSERT_PROC_NAME),
                        [ast.Comparison(
                            ast.RelOp.EQ,
                            call_to_run,
                            ast.LitteralBoolean(truth_vector[-1])
                        )]
                    ))
                    self.write(';'); self.newline()
                self.write('end {};'.format(conv_name(procedure_name)))
                self.newline()

    # This one is specific to the Ada language, since this is the main
    # testsuite language (the one used by test drivers.
    def serialize_run_module_interface(
        self, stream, target_language, truth_vectors
    ):
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
                    self.write('Pragma Import(C, {}, "{}");'.format(
                        conv_name(procedure_name), procedure_name
                    ))
                    self.newline()


    def serialize_specification_types(
        self, stream, types
    ):
        self.set_stream(stream)
        with PackageGuard(self, True, self.TYPES_MODULE):
            for type_decl in types:
                if not isinstance(type_decl, ast.BuiltinType):
                    self.handle(type_decl, declaration=True)

    def serialize_specification_program(
        self, stream, program, formal_names, formal_types
    ):
        self.set_stream(stream)

        self.add_use(self.TYPES_MODULE)
        self.newline()

        with PackageGuard(self, True, self.COMPUTING_MODULE):
            self.add_subprogram_signature(
                self.ENTRY_POINT_NAME, ast.BooleanType,
                formal_names, formal_types,
                True
            )

    def serialize_implementation(
        self, stream,
        program, formal_names, formal_types,
        one_operand_per_line
    ):
        self.set_stream(stream)

        self.one_operand_per_line = one_operand_per_line

        self.add_use(self.SUPPORT_MODULE)

        with PackageGuard(self, False, self.COMPUTING_MODULE):
            self.add_subprogram_signature(
                self.ENTRY_POINT_NAME, ast.BooleanType,
                formal_names, formal_types,
                False
            )
            self.handle(program)


    #
    # Various helpers
    #

    def add_use(self, module_name):
        self.write(
            'with {name}; use {name};'.format(name=conv_name(module_name))
        )
        self.newline()

    def add_subprogram_signature(self,
        name, return_type, formal_names, formal_types,
        declaration
    ):
        '''
        Add a subprogram signature to the output.

        If `return_type` is None, the subprogram is considered as a procedure.
        It is a function otherwise.
        '''
        # Add the type of subprogram and its name.
        self.write('{} {}'.format(
            'function' if return_type else 'procedure',
            conv_name(name),
        ))

        # Add the list of formals.
        if len(formal_names) > 0:
            self.write(' (')
            for i, (name, type_) in enumerate(
                zip(formal_names, formal_types)
            ):
                if i > 0:
                    self.write('; ')
                self.write('{} : '.format(conv_name(name)))
                self.handle(type_)
            self.write(')')

        # Add the return type, if any
        if return_type:
            self.write(' return ')
            self.handle(return_type)

        self.write(';' if declaration else ' is')
        self.newline()

    def handle_parent(self, expr):
        self.write('(')
        with self.indent():
            self.handle(expr)
        self.write(')')

    def helper_binop(self, op, left, right):
        self.handle_composite_expr(left)
        if self.one_operand_per_line and utils.contains_tag(left):
            self.newline()
        else:
            self.write(' ')
        self.write('{} '.format(op))
        self.handle_composite_expr(right)



    def format_comment(self, string):
        return '-- {}'.format(string)

    def handle_program(self, program, declaration=False):
        with self.indent(self.INDENT):
            for name, type_ in program.local_vars:
                self.write('{} : '.format(conv_name(name)));
                self.handle(type_)
                self.write(';')
                self.newline()

        self.write('begin'); self.newline()
        with self.indent(self.INDENT):
            for stmt in program.statements:
                self.handle(stmt)
        self.write('end;'); self.newline()


    #
    # Serialization for types
    #

    def handle_builtin_type(self, builtin_type, declaration=False):
        if declaration:
            raise ValueError(
                'Cannot output a type declaration for a builtin type'
            )
        else:
            self.write(BUILTIN_TYPES[builtin_type.name])

    def handle_record_type(self, record_type, declaration=False):
        if declaration:
            self.write('type {} is record'.format(conv_name(record_type.name)))
            self.newline()
            with self.indent(self.INDENT):
                for member in record_type.members:
                    self.handle(member)
            self.write('end record;')
            self.newline()
        else:
            self.write(conv_name(record_type.name))

    def handle_member_decl(self, member_decl):
        self.write('{} : '.format(conv_name(member_decl.name)))
        self.handle(member_decl.type)
        self.write(';')
        self.newline()


    #
    # Serialization for expressions
    #

    def handle_variable_usage(self, var):
        self.write('{}'.format(conv_name(var.name)))

    def handle_litteral_integer(self, integer):
        self.write('{}'.format(integer.value))

    def handle_litteral_boolean(self, boolean):
        # Python and Ada happen to share the same syntax for  litteral
        # booleans.
        self.write(str(boolean.value))

    def handle_litteral_record(self, record):
        self.write('(')
        for i, (member, decl) in enumerate(
            zip(record.members, record.type.members)
        ):
            if i > 0:
                self.write(', ')
            self.write('{} => '.format(conv_name(decl.name)))
            self.handle(member)
        self.write(')')

    def handle_comparison(self, comp):
        self.helper_binop(REL_OP[comp.operator], comp.left, comp.right)

    def handle_call(self, expr):
        self.handle_composite_expr(expr.function)
        if len(expr.arguments) > 0:
            self.write('(')
            with self.indent():
                for i, arg in enumerate(expr.arguments):
                    self.handle(arg)
                    if i + 1 < len(expr.arguments):
                        self.write(',')
                        self.newline()
            self.write(')')


    #
    # Serialization for topology expressions
    #

    def handle_and_expr(self, expr):
        self.helper_binop('and then', expr.left, expr.right)

    def handle_or_expr(self, expr):
        self.helper_binop('or else', expr.left, expr.right)

    def handle_not_expr(self, expr):
        self.write('not ')
        self.handle_composite_expr(expr.expr)

    def handle_composite_expr(self, expr):
        is_composite = isinstance(expr, (
            ast.Comparison,
            ast.And, ast.Or,
        ))

        if is_composite:
            self.write('(')
        with self.indent():
            self.handle(expr)
        if is_composite:
            self.write(')')


    #
    # Serialization for statements
    #

    def handle_if_stmt(self, stmt):
        self.write('if ')
        with self.indent():
            self.handle(stmt.condition)
        self.write(' then'); self.newline()

        with self.indent(self.INDENT):
            self.handle(stmt.true_stmt)

        self.write('else'); self.newline()
        with self.indent(self.INDENT):
            self.handle(stmt.false_stmt)
        self.write('end if;'); self.newline()

    def handle_while_stmt(self, stmt):
        self.write('while ')
        with self.indent():
            self.handle(stmt.condition)
        self.write(' loop'); self.newline()

        with self.indent(self.INDENT):
            self.handle(stmt.stmt)

        self.write('end loop;'); self.newline()

    def handle_return_stmt(self, stmt):
        self.write('return ')
        with self.indent():
            self.handle(stmt.expr)
        self.write(';')
        self.newline()

    def handle_assign(self, stmt):
        self.handle(stmt.variable)
        self.write(' := ')
        self.handle(stmt.expr)
        self.write(';')
        self.newline()
