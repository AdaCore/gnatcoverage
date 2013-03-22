# Another big chantier, exploring ideas around the possibility to generate
# parts of the testcase sources for libexp.

# Still extremely rough. Not used in production at all.

# Committed to facilitate communication between the developers involved.

import re

def body_lines (lines):
    return '\n'.join(lines) + '\n'

class Evaluator:
    def __init__(self, expr, context):
        self.expr = expr
        self.context = context

class Ada_Evaluator (Evaluator):

    def __init__(self, expr, context):
        Evaluator.__init__(self, expr, context)

    def package_spec(self):
        return (
            body_lines (["package This_Evaluator is"])
            + self.expr.predefs()
            + body_lines (["end;"])
            )

    def package_body(self):
        return (
            body_lines (["package body This_Evaluator is"])
            + self.proc_body()
            + body_lines (["end;"])
            )


    def proc_formals(self):
        return ', '.join (
            ["%s : %s" % (op.formal_name, op.formal_type) for op in expr.op])

    def proc_body_start(self):
        return body_lines (
            ["procedure Eval (%s) is" % (
                    self.proc_formals()),
             "begin"]
            )

    def proc_body_end(self):
        return body_lines (["end;"])

    def proc_body(self):
        return (
            self.proc_body_start()
            + self.context.body_for(expr=self.expr)
            + self.proc_body_end()
            )

class OP:
    def __init__(self):
        self.typedef = None
        self.formal_name = None
        self.formal_type = None
        self.actual_false = None
        self.actual_true = None

    def body(self):
        return "VAR_NOBODY"

class OP_Ada_Bool (OP):
    def __init__(self, formal_name):
        self.typedef = None
        self.formal_name = formal_name
        self.formal_type = "Boolean"
        self.actual_for = {
            False : "False",
            True  : "True"
            }

    def body(self):
        return self.formal_name

    @staticmethod
    def predefs():
        return body_lines (["-- no predefs for me"])

class EXPR:
    def __init__(self, opclasses):
        self.vectors = None
        self.next_arg_index = 0

        self.op = [
            opclass(formal_name=self.next_arg_name())
            for opclass in opclasses
            ]

        self.opclass_set = set (opclasses)

    def next_arg_name(self):
        this_arg_index = self.next_arg_index
        self.next_arg_index += 1
        return "arg%d" % this_arg_index

    def body(self):
        return "EXPR_NOBODY"

    def predefs(self):
        return '\n'.join (
            [opclass.predefs() for opclass in self.opclass_set]
            )

class EXPR_And(EXPR):
    def __init__(self, op0_class, op1_class):
        EXPR.__init__(self, opclasses=(op0_class, op1_class))
        self.vectors = ["fx_f", "tf_f", "tt_t"]

class EXPR_Ada_AndThen(EXPR_And):
    def __init__(self, op0_class, op1_class):
        EXPR_And.__init__(self, op0_class, op1_class)

    def body(self):
        return (
            "%s and then %s" % (self.op[0].body(), self.op[1].body())
            )

class EXPR_C_AndThen(EXPR_And):
    def __init__(self, op0, op1):
        EXPR_And.__init__(self, op0, op1)

    def body(self):
        return (
            "%s && %s" % (self.op[0].body(), self.op[1].body())
            )

class Context:
    pass

class CTX_AdaReturn (Context):

    def body_for(self, expr):
        return body_lines (
            ["return %s; -- # eval-all :e:" % expr.body()]
            )

class CTX_AdaIf (Context):

    def body_for(self, expr):
        return body_lines (
            ["if %s then       -- # eval-all :d:" % expr.body(),
             "  return True;   -- # on-true",
             "else",
             "  return False;  -- # on-false",
             "end if;"]
            )

class Tcgen:
    def __init__(self, xprname, opkinds, vectors, lang):
        self.xprname = xprname
        self.opkinds = opkinds
        self.vectors = vectors
        self.lang = lang

        # opkind/xprname+lang => class names
        # class object from string ? instantiate ?

        xpr_classname = "EXPR_%s_%s" % (self.lang, self.xprname)

        op_classnames = [
            "OP_%s_%s" % (self.lang, opk) for opk in self.opkinds]

expr = EXPR_Ada_AndThen (
    op0_class = OP_Ada_Bool,
    op1_class = OP_Ada_Bool
    )

for ctx in [CTX_AdaIf, CTX_AdaReturn]:
    aev = Ada_Evaluator (
        expr = expr, context=ctx()
        )
    print aev.package_spec()
    print aev.package_body()


# Vector triggers in target language as well

# test.py :
#   generate vector checkers
# src/test_ ... drivers with expectations
