# -*- coding: utf-8 -*-

'''
Ada-specific contexts
'''

import SCOV.expgen.ast      as ast
import SCOV.expgen.context  as context


class Case(context.LanguageSpecific):
    '''
    The decision expression is used as the controlling expression of a case
    statement.
    '''
    LANGUAGE    = 'Ada'
    TAG_CONTEXT = ast.TagTypes.EXPRESSION
    FORMAT      = [
        'case {decision_expr} is',
        '    when True =>',
        '        return True;  -- # on-true',
        '    when False =>',
        '        return False; -- # on-false',
        'end case;',
    ]

class DeclarationInitializer(context.LanguageSpecific):
    '''
    The decision expression is used as a local boolean initializer.
    '''
    LANGUAGE    = 'Ada'
    TAG_CONTEXT = ast.TagTypes.EXPRESSION
    FORMAT      = [
        'declare',
        '    E : Boolean := {decision_expr};'
        'begin',
        '    return E;',
        'end;',
    ]

class Discriminant(context.LanguageSpecific):
    '''
    The decision expression is used as a record discriminant.
    '''
    LANGUAGE    = 'Ada'
    TAG_CONTEXT = ast.TagTypes.EXPRESSION
    FORMAT      = [
        'declare',
        '    type My_Type (Value : Boolean) is null record;',
        '    R : My_Type (Value => {decision_expr});',
        'begin',
        '    return R.Value;',
        'end;',
    ]

class ExitWhen(context.LanguageSpecific):
    '''
    The decision expression is used as the condition for an EXIT WHEN
    statement.
    '''
    LANGUAGE    = 'Ada'
    TAG_CONTEXT = ast.TagTypes.DECISION
    FORMAT      = [
        'loop',
        '    exit when {decision_expr};',
        '    return False;      -- # on-false',
        'end loop;',
        'return True;           -- # on-true',
    ]

class For(context.LanguageSpecific):
    '''
    The decision expression is used as the controlling expression of a for
    statement.
    '''
    LANGUAGE    = 'Ada'
    TAG_CONTEXT = ast.TagTypes.EXPRESSION
    FORMAT      = [
        'declare',
        '    E : Boolean := False;',
        'begin',
        '    for Value in False .. ({decision_expr}) loop',
        '        E := Value;',
        '    end loop;',
        '    return E;',
        'end;',
    ]

class Index(context.LanguageSpecific):
    '''
    The decision expression is used as an array subscript.
    '''
    LANGUAGE    = 'Ada'
    TAG_CONTEXT = ast.TagTypes.EXPRESSION
    FORMAT      = [
        'declare',
        '    Values : array (Boolean) of Boolean := (',
        '        False => False,',
        '        True  => True',
        '    );',
        'begin',
        '    return Value ({decision_expr});',
        'end;',
    ]
