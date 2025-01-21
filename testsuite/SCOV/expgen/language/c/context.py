# -*- coding: utf-8 -*-

"""C-specific contexts"""

import SCOV.expgen.syntax as syntax
import SCOV.expgen.context as context


class DeclarationInitializer(context.LanguageSpecific):
    """The decision expression is used as a local boolean initializer."""

    LANGUAGE = "C"
    TAG_CONTEXT = syntax.TagTypes.EXPRESSION
    FORMAT = [
        "int result = {decision_expr};",
        "return result;",
    ]
