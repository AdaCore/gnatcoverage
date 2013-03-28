# -*- coding: utf-8 -*-

'''
Expose the base class for Language serializers and some formatting helpers.
'''

import StringIO

import SCOV.expgen.ast as ast
import SCOV.expgen.context as context
import SCOV.expgen.operand as operand
import SCOV.expgen.utils as utils


class Language(object):
    '''
    Base class for language serializers. To add a new language, subclass it.

    Subclasses have to define the `NAME` attribute to the name of the language,
    and they must define handlers for AST nodes (`handle_NODE_NAME` methods)
    that serialize those nodes to the formatter. Note that language-specific
    nodes (ast.X*) already have ready-to-use implementations here.

    They also have to define NotImplemented methods:
      - get_specification_filename
      - get_implementation_filename
      - serialize_test_procedure
      - serialize_specification_types
      - serialize_specification_program
      - serialize_implementation
      - format_comment
    '''

    NAME = None

    SUPPORT_MODULE      = 'support'
    # The "types" module is used to define types used by operands.
    TYPES_MODULE        = 'types'
    # The "run" module contains all "run_*" procedures called by test drivers.
    RUN_MODULE          = 'run'
    # These "run_*" functions call the "compute" function in the "computing"
    # module.
    COMPUTING_MODULE    = 'computing'
    ENTRY_POINT_NAME    = 'compute'
    ASSERT_PROC_NAME    = 'assert'

    # Used to format the name of the test procedures.
    BOOL_TO_CHAR = {
        False:  'F',
        True:   'T',
    }


    def __init__(self):
        self.one_operand_per_line = True


    #
    # Filename generators
    #

    def get_specification_filename(self, module_name):
        raise NotImplementedError()

    def get_implementation_filename(self, module_name):
        raise NotImplementedError()


    #
    # Serialization entry points
    #

    def serialize_run_module_implementation(
        self, stream, operand_kinds, truth_vectors
    ):
        '''
        Output the implementation of the run module, i.e. all "run_*"
        procedure. These procedures run the "compute" function with actuals
        that evaluate to the value specified in `truth_vector` for the given
        `operand_kinds`.

        `truth_vectors` is a list of lists of boolean: one list per "run_*"
        procedure, that contain one boolean per argument for the "compute"
        function, plus one for the expected result.
        '''
        raise NotImplementedError()

    def serialize_specification_types(
        self, stream, types
    ):
        '''
        Output a specification source that contains declarations for the given
        `types`. The name of the module for these specification is
        `TYPES_MODULE`.
        '''
        raise NotImplementedError()

    def serialize_specification_program(
        self, stream, formal_names, formal_types
    ):
        '''
        Output a specification source that contain a subprogram declaration for
        the "compute" function, given its formal names and types. The name of
        the module for these specification is `COMPUTING_MODULE`.
        '''
        raise NotImplementedError()

    def serialize_implementation(
        self, stream,
        program, formal_names, formal_types,
        one_operand_per_line
    ):
        '''
        Output an implementation source that contain the "compute" function,
        given its formal names and types. The name of the module for these
        specification is `COMPUTING_MODULE`.
        '''
        raise NotImplementedError()

    def handle(self, expr, *args, **kwargs):
        '''
        Dispatch handling of the given `expr` AST node to the corresponding
        handling method, passing it the *args and **kwargs arguments.
        '''
        arg_type = type(expr).__name__
        arg_handler = getattr(self, 'handle_{}'.format(arg_type))
        return arg_handler(expr, *args, **kwargs)

    def format_tree(self, tree, *args, **kwargs):
        '''
        Instead of serializing the given `tree` into the output stream, return
        the serialization as a string plus the resulting tag, if any.
        '''
        # Save the current formatter and replace it with a buffer one.
        output_buffer = StringIO.StringIO()
        formatter = self.formatter
        self.set_formatter(formatter.sub(output_buffer))

        # Let handlers serialize the `tree` into the buffer.
        self.handle(tree, *args, **kwargs)
        # Save the tag that remains in the buffer formatter, if any.
        remaining_tag = self.formatter.line_tag

        # Restore the old formatter.
        self.set_formatter(formatter)

        # And return the buffered serialization.
        return (output_buffer.getvalue(), remaining_tag)

    def format_comment(self, string):
        '''
        Return a line comment for the supported language that contains the
        given `string`.
        '''
        raise NotImplementedError()

    def handle_parent(self, node):
        '''
        Handle the given `node`, but add parenthesis around it.
        '''
        raise NotImplementedError()


    #
    # Various helpers
    #

    def set_stream(self, stream):
        '''
        Create a new formatter to wrap the given `stream` and use it.
        '''
        self.set_formatter(Formatter(self, stream))

    def set_formatter(self, formatter):
        '''
        Make the current Language instance use the given `formatter`.
        '''
        self.formatter = formatter

        # Shortcuts to very used formatter methods.
        self.indent = formatter.indent
        self.write = formatter.write
        self.newline = formatter.newline
        self.add_tag = formatter.add_tag

    def check_language(self, xnode):
        '''
        Assert that the given `xnode` handle the language that is handled by
        this instance.
        '''
        assert xnode.language == self.NAME, (
            '{} construct is specific to {} '
            'but is used with language {}'.format(
                type(xnode).__name__,
                xnode.language,
                self.NAME
            )
        )

    def handle_tagged_node(self, tagged_node):
        '''
        Add the tag of the given node and handle the nested node. Parenthesis
        are added if needed.
        '''
        self.add_tag(tagged_node.tag)
        # If the tagged node is an expression, add parenthesis in order to be
        # able to refer to this sub-expression from coverage expectations.
        if utils.is_expr(tagged_node.node):
            self.handle_parent(tagged_node.node)
        else:
            self.handle(tagged_node.node)

    def get_run_procedure_name(self, truth_vector):
        return 'run_{}_{}'.format(
            ''.join(
                self.BOOL_TO_CHAR[b]
                for b in truth_vector[:-1]
            ), # Actuals
            self.BOOL_TO_CHAR[truth_vector[-1]] # Result
        )


    #
    # Specific-language nodes (ast.X*) handlers
    #

    # Each of these check the language of the given node before using it...

    def handle_language_specific_type(self, xtype, declaration=False):
        self.check_language(xtype)
        if declaration:
            for line in xtype.declaration:
                self.write(line)
                self.newline()
        else:
            self.write(xtype.usage)

    def handle_language_specific_litteral(self, xlitteral):
        self.check_language(xlitteral)
        self.write(xlitteral.format)

    def handle_language_specific_operand(self, xoperand):
        self.check_language(xoperand)
        # Languages can process identifiers in a specific way.
        formal_name, remaining_tag = self.format_tree(
            ast.VariableUsage(xoperand.formal_name)
        )
        if remaining_tag:
            self.add_tag(remaining_tag)
        self.write(xoperand.format.format(formal_name=formal_name))

    def handle_language_specific_context(self, xcontext):
        self.check_language(xcontext)
        decision_expr, remaining_tag = self.format_tree(xcontext.decision_expr)
        for line in xcontext.format:
            if '{decision_expr}' in line:
                line = line.format(decision_expr=decision_expr)
                if remaining_tag:
                    self.add_tag(remaining_tag)
            self.write(line)
            self.newline()

    def _filter_nodes(self, specific_class, node_kinds):
        '''
        Return the subset of `node_kinds` that can be used with this language,
        `specific_class` being the kind of nodes that can be specific to
        a language.
        '''
        return [
            node_kind
            for node_kind in node_kinds
            if (
                not isinstance(node_kind, specific_class)
                or node_kind.language == self.NAME
            )
        ]

    def filter_contexts(self, contexts):
        '''
        Return the subset of `contexts` that can be used with this language.
        '''
        return self._filter_nodes(context.LanguageSpecific, contexts)

    def filter_operand_kinds(self, operand_kinds):
        '''
        Return the subset of `operand_kinds` that can be used with this
        language.
        '''
        return self._filter_nodes(operand.LanguageSpecific, operand_kinds)

    def filter_types(self, types):
        '''
        Return the subset of `types` that can be used with this language.
        '''
        return self._filter_nodes(ast.XType, types)


#
# Formatting helper classes
#

class IndentationGuard(object):
    '''
    Increment the indentation level on entry and decrement it when leaving.
    '''
    def __init__(self, formatter, addend):
        self.formatter = formatter
        self.addend = addend

    def __enter__(self):
        self.formatter.push_indent(self.addend)

    def __exit__(self, type, value, traceback):
        self.formatter.pop_indent()

class Formatter(object):
    '''
    Output stream wrapper that takes care of indentation, line tags, etc.
    '''

    def __init__(self, language, stream):
        self.language = language
        self.stream = stream

        # Current line number of characters
        self.current_column = 0
        # Identation levels stack. Topmost (self.indent_stack[-1]) will be the
        # indentation level of the next line.
        self.indent_stack = [0]

        # Tag for the current line. Flushed on newline.
        self.line_tag = None

    def sub(self, stream):
        '''
        Return a new formatter that have a "sub-state" (same indentation state,
        but no duplicated tag), with another output stream.
        '''
        result = Formatter(self.language, stream)
        result.current_column = self.current_column
        result.indent_stack = list(self.indent_stack)
        result.line_tag = None
        return result

    def write(self, string):
        '''
        Write some `string` into the wrapped stream.

        Take care of column handling. The given `string` must not contain a new
        line character.
        '''
        if self.current_column == 0:
            self.current_column = self.indent_stack[-1]
            self.stream.write(' ' * self.indent_stack[-1])
        self.stream.write(string)
        self.current_column += len(string)

    def push_indent(self, addend=0):
        '''
        Add an identation level.

        The new level is based on the current column plus `addend` columns.
        '''
        old_level = self.current_column or self.indent_stack[-1]
        self.indent_stack.append(old_level + addend)

    def pop_indent(self):
        '''
        Pop the newest indentation level.
        '''
        self.indent_stack.pop()

    def indent(self, addend=0):
        '''
        Create and return an identation guard.
        '''
        return IndentationGuard(self, addend)

    def newline(self):
        '''
        Flush any tag and insert a new line character.
        '''
        self.flush_tags()
        self.stream.write('\n')
        self.current_column = 0

    def add_tag(self, tag):
        '''
        Add a `tag` to the current line.
        '''
        utils.check_tag(tag)
        if self.line_tag:
            if tag.name != self.line_tag.name:
                raise ValueError(
                    'Trying to insert a `{}` tag on a line where there is a '
                    '`{}` tag'.format(tag.name, self.line_tag.name)
                )
            if tag.context != self.line_tag.context:
                raise ValueError(
                    'Trying to insert a `{}` tag on a line where there is a '
                    '`{}` tag'.format(tag.context, t.context)
                )
            tag = tag._replace(operand='all')
        self.line_tag = tag

    def flush_tags(self):
        '''
        Flush any tag. Should be called only through the `newline` method.
        '''
        if self.line_tag is not None:
            comment = self.language.format_comment(
                utils.format_tag(self.line_tag)
            )
            self.stream.write(' {}'.format(comment))
            self.line_tag = None
