****************************
Glossary of terms & concepts
****************************

.. glossary::

   Complex Boolean Expressions
   Complex Boolean Expression
      Boolean expression with at least two operands, where all the binary
      operators are defined to enforce short-circuit semantics.
      
   Coverage Violations
   Coverage Violation
      Failure to satisfy a particular requirement of a coverage criterion.
      For example, not executing a given statement is a failure to satisfy
      part of the Statement Coverage criterion requirements, triggering a
      "statement not executed" violation of the criterion.

   Conditions
   Condition
      Leaf operand, possible unary operator included, in a Decision
      expression. For example, ``A``, ``not B`` and ``C`` in ``if A or else
      (not B and then C) then``.

   Decision
      Any Simple or Complex Boolean expression used as the control value
      in a source level control-flow oriented statement (*if*, *while*, *for*).
      For MCDC analysis purposes, we also treat as decisions Complex Boolean
      Expressions anywhere they appear, for example also on the right hand
      side of an assignment.
      
   Evaluation Vector
      A set of Boolean values assigned to each :term:`condition` of a
      :term:`decision` together with the resulting expression value. For example
      A=True, B=False, Expression=False for ``A and then B``.

   Exemption Regions
   Exemption Region
      Regions of source for which :term:`coverage violations` are expected,
      and explicitly marked as such with a mandatory justification text.

   Independence Pairs
   Independence Pair
      A pair of decision evaluation vectors that demonstrate the independent
      influence of a specific condition on the decision for MCDC analysis purposes.
      The set of valid pairs for a condition depends on the particular MCDC variant
      driving the analysis.

   Simple Boolean Expression
      Boolean expression with a single operand, possibly negated. No binary
      operator.

   Source Coverage Obligations
   Source Coverage Obligation
      Item that designates a source program entity which must be subject
      to coverage checks for a set of criteria. This conveys an entity kind,
      for example *statement*, *decision* or *condition*, together with a
      specific unit:line:column source location. Also known as SCO.

   Test Driver
      Part of a program that is used to exercise a set of application units
      in a specific manner, to test conformance of these units to particular
      requirements. Coverage objectives are typically associated with the
      applicative part only.

   @listfile arguments
      Part of a command line that designates a file which contains a
      list of items, one per line in the file. The expected nature of each
      item depends on the context.

