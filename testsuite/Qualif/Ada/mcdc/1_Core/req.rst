Core MCDC requirements
======================

.. rubric:: Requirement(s)

For MCDC assessments, the tool focuses on decisions used to influence
control-flow languages constructs as well as on those with more than one
condition anywhere they might appear. Only short circuit operators are
allowed to combine conditions.

All the DC rules apply unchanged to the full set of decisions considered
here. Rule #3, about decisions evaluated both True and False, is complemented
by an additional rule:

======  =====================================================================
Rule #  Description
======  =====================================================================
3c      For expressions evaluated both True and False, the tool shall report
        every condition for which the independent effect was not
        demonstrated. Such condition specific diagnotics shall designate the
        particular condition source location.
======  =====================================================================


.. rubric:: Testing Strategy

The testing strategy is similar to the one chosen for the DC core requirement,
with the following set of testcases:


.. qmlink:: SubsetIndexImporter

   *

Rules 1 to 3c are validated by variations exercised in every individual
testcase, where we consistenly check each decision of interest in multiple
manners, always including:

* a situation where the statements exposing the decision aren't
  executed at all (*rule #1*),

* a set of vectors where the decision evaluates only True (*rule #2*),

* a set of vectors where the decision evaluates only False (*rule #2*),

* sets of vectors where the decision evaluates both True and False, with
  at least

  * one set not demonstrating the independent effect of any condition
    (*rules #3 and 3c*),

  * one set demonstrating the independent effect of each condition alone
    (*rules #3 and 3c*),

  * one set demonstrating the independent effect of all the conditions
    (*rule #3 and 3c*).

Rules #4 and #5 are addressed by the organization of the set of testcase
groups presented in the table above.
