****************************
Glossary of terms & concepts
****************************

.. glossary::

   Simple Boolean Expression
      Boolean expression with a single operand, possibly negated. No binary
      operator.

   Complex Boolean Expression
      Boolean expression with at least two operands, where all the binary
      operators are defined to enforce short-circuit semantics.
      
   Source Coverage Obligation
      Item that designates a source program entity which must be subject
      to coverage checks for a set of criteria. This conveys an entity kind,
      for example *statement*, *decision* or *condition*, together with a
      specific unit:line:column source location.

   Source Coverage Obligations
      A set of Source Coverage Obligation items. Also known as SCOs.

   Exemption Regions
      Regions of source for which :term:`coverage violations` are expected,
      and explicitly marked as such with a mandatory justification text.

   Coverage Violations
      Failures to satisfy a particular requirement of a coverage criterion.
      For example, not executing a given statement is a failure to satisfy
      part of the Statement Coverage criterion requirements, triggering a
      "statement not executed" violation of the criterion.

   @listfile arguments
      Part of a command line that designates a file which contains a
      list of items, one per line in the file. The expected nature of each
      item depends on the context.

