**sequences potentially skipped by exceptions that propagate through multiple levels of block nesting**

Check SC of statement sequences potentially skipped by exceptions
that propagate through multiple levels of block nesting within a
subprogram body.

Because of the ZFP limitation on exception propagation,
the only case to check is nested block statements.

