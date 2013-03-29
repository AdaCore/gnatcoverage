# -*- coding: utf-8 -*-

'''
Ada-specific operands
'''

import SCOV.expgen.ast as ast
import SCOV.expgen.operand as operand


#
# Types
#

# Bultin ones
INTEGER = ([], 'Integer')
STRING = ([], 'String')

BOOLEAN_SUBTYPE = (
    ['subtype Bool_Subtype is Boolean;'], # Type declaration
    'Bool_Subtype' # Type usage
)
BOOLEAN_TYPE = (
    ['type Bool_Type is new Boolean;'], # Type declaration
    'Bool_Type' # Type usage
)
BOOLEAN_ACTUALS = {False: 'False', True: 'True'}

SLOC_RECORD = (
    [ # Type declaration
        'type Sloc is record',
        '    Line : Integer;',
        '    Column : Integer;',
        'end record;',
    ],
    'Sloc' # Type usage
)

TWO_STRINGS = (
    [ # Type declaration
        'type Two_Strings (L1, L2 : Integer) is record',
        '    First : String (1 .. L1);',
        '    Second : String (1 .. L2);',
        'end record;',
    ],
    'Two_Strings' # Type usage
)

SENSOR = (
    [ # Type declaration
        'type Sensor is record',
        '    Low, High : Integer;',
        '    Value : Integer;',
        'end record;',
    ],
    'Sensor' # Type usage
)


class Aggregate(operand.LanguageSpecific):
    '''
    Compare the argument with some litteral aggregate.
    '''
    LANGUAGE    = 'Ada'
    USED_TYPES  = (SLOC_RECORD, )
    PARAM_TYPE  = SLOC_RECORD
    FORMAT      = '{formal_name} = (Line => 1, Column => 2)'
    ACTUALS     = {
        False:  '(Line => 1, Column => 3)',
        True:   '(Line => 1, Column => 2)',
    }

class Component(operand.LanguageSpecific):
    '''
    Compare the member of a structure with a litteral integer.
    '''
    LANGUAGE    = 'Ada'
    USED_TYPES  = (SLOC_RECORD, )
    PARAM_TYPE  = SLOC_RECORD
    FORMAT      = '{formal_name}.Line = 1'
    ACTUALS     = {
        False:  '(Line => 2, Column => 1)',
        True:   '(Line => 1, Column => 1)',
    }

class LengthAttribute(operand.LanguageSpecific):
    '''
    Compare the length of the argument with a constant.
    '''
    LANGUAGE    = 'Ada'
    USED_TYPES  = (STRING, )
    PARAM_TYPE  = STRING
    FORMAT      = "{formal_name}'Length > 0"
    ACTUALS     = {
        False:  '""',
        True:   '"Hello, world!"',
    }

class StringConcatenation(operand.LanguageSpecific):
    '''
    Compare the concatenation of two strings with a litteral string.
    '''
    LANGUAGE    = 'Ada'
    USED_TYPES  = (STRING, TWO_STRINGS, )
    PARAM_TYPE  = TWO_STRINGS
    FORMAT      = (
        '({formal_name}.Second & {formal_name}.First)'
        ' = "Hello, world!"'
    )
    ACTUALS     = {
        False:  '(Second => "I beg you", L2 => 9,'
                ' First =>  "pardon?",   L1 => 7)',
        True:   '(Second => "Hello, ",   L2 => 7,'
                ' First =>  "world!",    L1 => 6)',
    }

class StringSlice(operand.LanguageSpecific):
    '''
    Compare the slice of a string with a litteral string.
    '''
    LANGUAGE    = 'Ada'
    USED_TYPES  = (STRING, )
    PARAM_TYPE  = STRING
    FORMAT      = (
        '{formal_name}( {formal_name}\'First .. {formal_name}\'Last - 1)'
        ' = "Hell"'
    )
    ACTUALS     = {
        False:  '"World"',
        True:   '"Hello"',
    }

class Modulo(operand.LanguageSpecific):
    '''
    Compare the modulo of the argument with a litteral integer.
    '''
    LANGUAGE    = 'Ada'
    USED_TYPES  = (INTEGER, )
    PARAM_TYPE  = INTEGER
    FORMAT      = '{formal_name} mod 17 = 0'
    ACTUALS     = {
        False:  '18',
        True:   '34',
    }

class Range(operand.LanguageSpecific):
    '''
    Test whether an integer is in a range.
    '''
    LANGUAGE    = 'Ada'
    USED_TYPES  = (SENSOR, )
    PARAM_TYPE  = SENSOR
    FORMAT      = (
        '{formal_name}.Value in '
        '{formal_name}.Low .. {formal_name}.High'
    )
    ACTUALS     = {
        False:  '(Low => 0, High => 10, Value => 15)',
        True:   '(Low => 0, High => 10, Value => 10)',
    }

class Subtype(operand.LanguageSpecific):
    '''
    Use a subtype of Boolean as an operand.
    '''
    LANGUAGE    = 'Ada'
    USED_TYPES  = (BOOLEAN_SUBTYPE, )
    PARAM_TYPE  = BOOLEAN_SUBTYPE
    FORMAT      = '{formal_name}'
    ACTUALS     = BOOLEAN_ACTUALS

class DerivedType(operand.LanguageSpecific):
    '''
    Use a derived type of Boolean as an operand.
    '''
    LANGUAGE    = 'Ada'
    USED_TYPES  = (BOOLEAN_TYPE, )
    PARAM_TYPE  = BOOLEAN_TYPE
    FORMAT      = 'Boolean({formal_name})'
    ACTUALS     = BOOLEAN_ACTUALS
