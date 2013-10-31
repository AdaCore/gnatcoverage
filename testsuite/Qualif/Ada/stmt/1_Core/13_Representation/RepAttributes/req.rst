Attribute Definition Clauses for Representation Attributes
==========================================================

Attribute definition clauses for representation attributes are
of interest in that some of them allow the evaluation of expressions
with possible side effects.

.. rubric:: Requirement(s)

Attribute definition clauses for representation attributes aren't statements
in the language definition sense and shall not be considered as such by the
tool.

The side effects of expression evaluations incurred by such clauses (for
example subprogram calls) shall be accounted for in the coverage assessments
performed by the tool.

.. rubric:: Testing Strategy

Most representation attributes accept only static expressions, except
'Address. We check the tool behavior on address clauses set on local or
library level variables, with expressions involving calls to a subprogram with
conditional parts.

In both the local and library level cases, check that the subpgrogram parts
not exercised by address clauses on elaborated variables are reported
uncovered.

.. qmlink:: TCIndexImporter

   *

