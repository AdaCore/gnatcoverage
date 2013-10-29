**Exercise sequences of multiple no-op statements (e.g. null or pragma)**

Exercise a subprogram with two consecutive statements not generating code
(e.g. null statement or pragma) followed by a statement with code.

Verify that the same coverage is reported for all the statements, when the
subprogram is called as well as when not.

