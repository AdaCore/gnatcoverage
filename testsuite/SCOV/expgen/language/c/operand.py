"""C-specific operands"""

from __future__ import annotations

import SCOV.expgen.operand as operand
import SCOV.expgen.syntax as syntax


#
# Types
#

# Builtin types
INTEGER = syntax.XType("C", (), "int")

SLOC_STRUCT = syntax.XType(
    "C",
    (
        # Type declaration
        "struct sloc",
        "{",
        "    int line;",
        "    int column;",
        "};",
    ),
    "struct sloc",  # Type usage
)


class Component(operand.LanguageSpecific):
    """Compare a field of a structure with a litteral integer."""

    LANGUAGE = "C"
    USED_TYPES = (SLOC_STRUCT,)
    PARAM_TYPE = SLOC_STRUCT
    FORMAT = "{formal_name}.line == 1"
    ACTUALS = {
        False: "(struct sloc) {2, 1}",
        True: "(struct sloc) {1, 1}",
    }


class Modulo(operand.LanguageSpecific):
    """Compare the modulo of the arguent with a litteral integer."""

    LANGUAGE = "C"
    USED_TYPES = (INTEGER,)
    PARAM_TYPE = INTEGER
    FORMAT = "{formal_name} % 17 == 0"
    ACTUALS = {
        False: "18",
        True: "34",
    }
