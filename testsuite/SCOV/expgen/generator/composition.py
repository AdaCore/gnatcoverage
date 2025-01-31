# -*- coding: utf-8 -*-

"""Set of composition elements."""


import SCOV.expgen.syntax as syntax
import SCOV.expgen.context as context
import SCOV.expgen.language as language
import SCOV.expgen.language.ada.context as ada_context
import SCOV.expgen.language.ada.operand as ada_operand
import SCOV.expgen.language.c.context as c_context
import SCOV.expgen.language.c.operand as c_operand
import SCOV.expgen.operand as operand


languages = [lang_mod.Language() for lang_mod in (language.ada, language.c)]

operand_kinds = [
    operand.Variable(),
    operand.IntegerComparison(syntax.RelOp.GT, 1),
    # Ada-specific operands
    ada_operand.Aggregate(),
    ada_operand.Component(),
    ada_operand.LengthAttribute(),
    ada_operand.StringConcatenation(),
    ada_operand.StringSlice(),
    ada_operand.Modulo(),
    ada_operand.Subtype(),
    # TODO: find how to make coverage expectation match the whole expression
    # (including the Boolean conversion operator).
    # -> ada_operand.DerivedType(),
    # C-specific operands
    c_operand.Component(),
    c_operand.Modulo(),
]

contexts = [
    context.Call(),
    context.If(),
    context.While(),
    context.Return(),
    # Ada-specific contexts
    ada_context.Case(),
    ada_context.DeclarationInitializer(),
    ada_context.Discriminant(),
    ada_context.ExitWhen(),
    ada_context.For(),
    ada_context.Index(),
    # C-specific contexts
    c_context.DeclarationInitializer(),
]
