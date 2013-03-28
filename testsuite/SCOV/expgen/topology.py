# -*- coding: utf-8 -*-

'''
Expose the topology definition and instanciation engine.

A topology is used to specify the pattern of a decision: X and (not Y or Z),
for instance, is such a pattern, built from a boolean expression and
placeholders.

Once a topology is defined, it can be instanciated to a full decision
expression: to do so, placeholders are replaced by "operand" expressions.
'''

import SCOV.expgen.ast as ast


class OperandPlaceholder(object):
    '''
    Placeholder to be put in expressions to specify a topology.
    '''
    pass


class Topology(object):
    '''
    Decision expression pattern that can be instanciated to fill the pattern
    with actual operands.
    '''

    def __init__(self, expression):
        '''
        Create a new topology using `expression` as its pattern.
        '''
        self.expression = expression
        self.arity = self.get_arity(expression)

    def get_arity(self, expression):
        '''
        Return the number of operand placeholders in the given expression.
        '''
        if isinstance(expression, OperandPlaceholder):
            return 1
        else:
            return sum(
                self.get_arity(sub_expr)
                for sub_expr in expression
            )

    def instanciate(self, operands, formal_names, context):
        '''
        Instanciate the expression pattern tree replacing the placeholders with
        the given expressions, which are tagged with the given `formal_names`
        and with the given `context` (tags depend on the context).
        '''

        def helper(expression, i):
            '''
            Likewise, helper to process sub-expressions in a recursive fashion.
            `i` is the first operand to replace. Return the instanciated
            expression and the index of the next operand to replace.
            '''

            # If we find a placeholder, return the next operand after tagging
            # it.
            if isinstance(expression, OperandPlaceholder):
                result = ast.TaggedNode(
                    ast.Tag('eval', formal_names[i], context.TAG_CONTEXT),
                    operands[i]
                )
                return (result, i + 1)

            # Otherwise, first instanciate sub-expressions of the given
            # expression pattern.
            sub_expressions = []
            for sub_expr in expression:
                instance, next_operand = helper(sub_expr, i)
                i = next_operand
                sub_expressions.append(instance)

            # And then return the instanciated expression.
            ExprKind = type(expression)
            return ExprKind(*sub_expressions), i

        # The number of operands must naturally match the number of
        # placeholders in the expression pattern.
        assert len(operands) == self.arity

        expr, next_operand = helper(self.expression, 0)

        # All operands must have be replaced.
        assert next_operand == self.arity

        return expr


    def evaluate(self, operands):
        '''
        Evaluate the boolean expression using the given `operands`.
        '''

        def helper(expression, i):
            '''
            Likewise, helper to process sub-expressions in a recursive fashion.
            `i` is the first `operand` to use. Return the result of the given
            `expression` evaluation and the index of the next operand to use.
            '''

            # If we find a placeholder, return the next operand.
            if isinstance(expression, OperandPlaceholder):
                return (operands[i], i + 1)

            # Otherwise, evaluate sub-expressions and return the result.
            if isinstance(expression, ast.And):
                left, i = helper(expression.left, i)
                right, i = helper(expression.right, i)
                return (left and right, i)
            elif isinstance(expression, ast.Or):
                left, i = helper(expression.left, i)
                right, i = helper(expression.right, i)
                return (left or right, i)
            elif isinstance(expression, ast.Not):
                result, i = helper(expression.expr, i)
                return (not result, i)
            else:
                raise ValueError(
                    'Invalid topology node: {}'.format(expression)
                )

        # The number of operands must naturally match the number of
        # placeholders in the expression pattern.
        assert len(operands) == self.arity

        result, next_operand = helper(self.expression, 0)

        # All operands must have be replaced.
        assert next_operand == self.arity

        return result


    def __str__(self):

        placeholders = list(reversed('ABCDEFGHIJ'))

        def helper(expr):
            if isinstance(expr, OperandPlaceholder):
                return placeholders.pop()
            elif isinstance(expr, ast.And):
                return '({} and {})'.format(
                    helper(expr.left), helper(expr.right)
                )
            elif isinstance(expr, ast.Or):
                return '({} or {})'.format(
                    helper(expr.left), helper(expr.right)
                )
            else:
                return 'not {}'.format(helper(expr.expr))

        return helper(self.expression)
