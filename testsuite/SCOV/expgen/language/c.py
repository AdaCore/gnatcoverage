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
    ast.RelOp.EQ: '==',
    ast.RelOp.NE: '!=',
}

# Serialization for builtin types
BUILTIN_TYPES = {
    ast.Types.BOOLEAN:  'int',
    ast.Types.INTEGER:  'int',
}


class HeaderGuard(object):
    '''
    Output C header guards on entry and exit.
    '''

    def __init__(self, language, module_name):
        self.language = language
        self.module_name = module_name

    def __enter__(self):
        self.language.write('#ifndef {}_H'.format(self.module_name.upper()))
        self.language.newline()
        self.language.write('#define {}_H'.format(self.module_name.upper()))
        self.language.newline()

    def __exit__(self, type, value, traceback):
        self.language.write('#endif')
        self.language.newline()


class Language(language.Language):

    NAME = 'C'
    INDENT = 2

    RUN_MODULE = 'run_body'


    #
    # Filename generators
    #

    def get_specification_filename(self, module_name):
        return '{}.h'.format(module_name)

    def get_implementation_filename(self, module_name):
        return '{}.c'.format(module_name)


    #
    # Serialization entry points
    #

    def serialize_run_module_implementation(
        self, stream, operand_kinds, truth_vectors
    ):
        self.set_stream(stream)

        self.add_include(self.SUPPORT_MODULE)
        self.add_include(self.TYPES_MODULE)
        self.add_include(self.COMPUTING_MODULE)
        self.newline()

        # For each truth vector, generate a run procedure.
        for i, truth_vector in enumerate(truth_vectors):
            if i > 0:
                self.newline()

            self.add_subprogram_signature(
                self.get_run_procedure_name(truth_vector),
                None, [], [], False
            )
            self.write('{'); self.newline()
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
            self.write('}'); self.newline()

    def serialize_specification_types(
        self, stream, types
    ):
        self.set_stream(stream)
        with HeaderGuard(self, self.TYPES_MODULE):
            for type_decl in types:
                if not isinstance(type_decl, ast.BuiltinType):
                    self.handle(type_decl, declaration=True)

    def serialize_specification_program(
        self, stream, program, formal_names, formal_types
    ):
        self.set_stream(stream)
        with HeaderGuard(self, self.COMPUTING_MODULE):
            self.add_include(self.TYPES_MODULE)
            self.newline()

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

        self.add_include(self.SUPPORT_MODULE)
        self.add_include(self.TYPES_MODULE)
        self.add_include(self.COMPUTING_MODULE)
        self.newline()

        self.add_subprogram_signature(
            self.ENTRY_POINT_NAME, ast.BooleanType,
            formal_names, formal_types,
            False
        )
        self.write('{'); self.newline()
        self.handle(program)
        self.write('}'); self.newline()


    #
    # Various helpers
    #

    def add_include(self, module_name):
        self.write('#include "{}.h"'.format(module_name))
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
        # Add subprogram return type, prepend subprogram linkage if needed.
        if declaration:
            self.write('extern ')
        if return_type:
            self.handle(return_type)
        else:
            self.write('void')
        self.newline()

        # Then add the name of the subprogram and its arguments.
        self.write('{} ('.format(name))
        for i, (name, type_) in enumerate(
            zip(formal_names, formal_types)
        ):
            if i > 0:
                self.write(', ')
            self.handle(type_)
            self.write(' {}'.format(name))
        if len(formal_names) == 0:
            self.write('void')
        self.write(')')

        if declaration:
            self.write(';')
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
        return '// {}'.format(string)

    def handle_program(self, program, declaration=False):
        with self.indent(self.INDENT):
            for name, type_ in program.local_vars:
                self.handle(type_)
                self.write(' {};'.format(name));
                self.newline()

            for stmt in program.statements:
                self.handle(stmt)


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
            self.write('struct {}'.format(record_type.name))
            self.newline()
            self.write('{')
            self.newline()
            with self.indent(self.INDENT):
                for member in record_type.members:
                    self.handle(member)
            self.write('};')
            self.newline()
        else:
            self.write('struct {}'.format(record_type.name))

    def handle_member_decl(self, member_decl):
        self.handle(member_decl.type)
        self.write(' {};'.format(member_decl.name))
        self.newline()


    #
    # Serialization for expressions
    #

    def handle_variable_usage(self, var):
        self.write('{}'.format(var.name))

    def handle_litteral_integer(self, integer):
        self.write('{}'.format(integer.value))

    def handle_litteral_boolean(self, boolean):
        self.write('{}'.format(
            {True: 1, False: 0}[boolean.value]
        ))

    def handle_litteral_record(self, record):
        self.write('(')
        self.handle(record.type)
        self.write(') {')
        for i, member in enumerate(record.members):
            if i > 0:
                self.write(', ')
            self.handle(member)
        self.write('}')

    def handle_comparison(self, comp):
        self.helper_binop(REL_OP[comp.operator], comp.left, comp.right)

    def handle_call(self, expr):
        self.handle_composite_expr(expr.function)
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
        self.helper_binop('&&', expr.left, expr.right)

    def handle_or_expr(self, expr):
        self.helper_binop('||', expr.left, expr.right)

    def handle_not_expr(self, expr):
        self.write('!')
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
        self.write('if (')
        with self.indent():
            self.handle(stmt.condition)
        self.write(')'); self.newline()

        self.write('{'); self.newline()
        with self.indent(self.INDENT):
            self.handle(stmt.true_stmt)
        self.write('}'); self.newline()

        self.write('else'); self.newline()
        self.write('{'); self.newline()
        with self.indent(self.INDENT):
            self.handle(stmt.false_stmt)
        self.write('}'); self.newline()

    def handle_return_stmt(self, stmt):
        self.write('return ')
        with self.indent():
            self.handle(stmt.expr)
        self.write(';')
        self.newline()

    def handle_assign(self, stmt):
        self.handle(stmt.variable)
        self.write(' = ')
        self.handle(stmt.expr)
        self.write(';')
        self.newline()
