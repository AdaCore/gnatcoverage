Check SC on expressions that can fail to evaluate because of exceptions
=========================================================================

Check the effects of an expression evaluation interrupted by an exception
raise, for a set of expressions and a set of possible exception origins, as
part of a simple statement or in the control expression in compound
statements.

For each variant, check a variety of situations where

* No exception gets raised at all

* An exception gets raised as part of all the evaluation attempts,
  on the first or the last operand evaluation,

* An exception would be raised by one operand but is not because of
  the shortcircuit semantics,

* An exception is raised by some evaluation but not others


.. qmlink:: TCIndexImporter

   *





