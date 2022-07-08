****************************
Glossary of terms & concepts
****************************

.. glossary::

   Binary Trace
     File containing low level information about the blocks of machine
     code executed by a program, from which |gcp| can infer source level
     coverage assessments. Such traces are produced by an instrumented
     execution environment for programs built from their original sources.

   Complex Boolean Expression
      Boolean expression with at least two operands, where all the binary
      operators are defined to enforce short-circuit semantics.

   Coverage Violation
      Failure to satisfy a particular requirement of a coverage criterion.
      For example, not executing a given statement is a failure to satisfy
      part of the Statement Coverage criterion requirements, triggering a
      "statement not executed" violation of the criterion.

   Condition
      Leaf operand, possible unary operator included, in a Decision
      expression. For example, ``A``, ``not B`` and ``C`` in ``if A or else
      (not B and then C) then``.

   Coverage Checkpoint
      File a condensed version of a coverage assessment computed by |gcvcov|,
      which the tool can reload and combine with other assessments very
      efficiently. This may be viewed as an internal representation of a
      coverage report which :cmd-option:`--annotate` would output in a user
      readable form. See :ref:`checkpoints` for more information.

   Coverage Runtime
      Library of services used by instrumented sources to manipulate
      coverage data while the program is running and produce a source
      trace file from such data at user selectable execution points.

   Decision
      Any Simple or Complex Boolean expression used as the control value
      in a source level control-flow oriented statement (*if*, *while*, *for*).
      For MCDC analysis purposes, we also treat as decisions Complex Boolean
      Expressions anywhere they appear, for example also on the right hand
      side of an assignment.

   Evaluation Vector
      A set of Boolean values assigned to each :term:`condition <Condition>` of
      a :term:`decision <Decision>` together with the resulting expression
      value.  For example A=True, B=False, Expression=False for ``A and then
      B``.

   Exemption Region
      Regions of source for which :term:`coverage violations <Coverage
      Violation>` are expected, and explicitly marked as such with a mandatory
      justification text.

   Incremental Coverage Analysis
      The process of constructing a consolidated coverage result incrementally,
      using a single checkpoint file to accumulate new info about discharged
      coverage obligations as tests execute in sequence. See
      :ref:`incremental_analysis`.

   Independence Pair
      A pair of decision evaluation vectors that demonstrate the independent
      influence of a specific condition on the decision for MCDC analysis
      purposes.
      The set of valid pairs for a condition depends on the particular MCDC
      variant driving the analysis.

   Library Information file
      A text file produced by the compiler as it processes a source unit,
      containing information about the entities present in the unit; for
      example: dependencies w.r.t. other units, cross-reference information,
      Source Coverage Obligations, annotation pragmas for exemption regions,
      ...  These are ``.ali`` files for Ada and ``.gli`` files for C, produced
      at the same place as the object file for a given compilation.

   Simple Boolean Expression
      Boolean expression with a single operand, possibly negated. No binary
      operator.

   Source Coverage Obligation
      Item that designates a source program entity which must be subject
      to coverage checks for a set of criteria. This conveys an entity kind,
      for example *statement*, *decision* or *condition*, together with a
      specific unit:line:column source location. Also known as SCO.

   Source Instrumentation Data file
      A binary file produced by the gnatcov instrumenter as it processes a
      source unit, containing information about the entities present in the
      unit; for example: Source Coverage Obligations, annotation pragmas for
      exempted regions, ... These are ``.sid`` files, produced at the same
      place object files would be created during the compilation of the unit.

   Source Trace
     File(s) containing high level information about the source coverage
     achievements resulting from a program execution. Such traces are
     produced by an instrumented version of the program running in its
     regular execution environment.

   Test Driver
      Part of a program that is used to exercise a set of application units
      in a specific manner, to test conformance of these units to particular
      requirements. Coverage objectives are typically associated with the
      applicative part only.

   Units of Interest
      Set of units on which coverage should be assessed. See :ref:`sunits` for
      more information.

   @listfile argument
      Part of a command line that designates a text file which contains a
      list of items, one per line in the file. The expected nature of each
      item depends on the context.

