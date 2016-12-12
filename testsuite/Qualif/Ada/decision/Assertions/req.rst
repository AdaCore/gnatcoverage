DC requirements regarding Assertions
====================================

By *Assertions* we designate all the Boolean expressions stating properties
expected to hold in nominal execution conditions. This encompasses expressions
controlling local Assert pragmas or more general contract based programming
pragmas or aspects (Pre or Post conditions attached to subprograms, subtype
predicates, type invariants).

Assertions are expected never to evaluate False so call for specific coverage
criteria other than the traditional Decision Coverage definition. In absence
of such criteria, the tool is not qualified to perform any kind of coverage
assessment on active assertions, as controlled by *Assertion_Policy* values.

There shall be no Decision Coverage obligation attached to assertion aspects
or pragmas for which the controlling expression is never evaluated (assertion
policy set to "ignore" or "disable" for Pre/Post, Assert or type invariants,
or to "disable" for subtype predicates); the coverage assessments shall
proceed as if the assertions weren't there.

.. rubric:: Testing Strategy

We verify the correctness of the statement and decision coverage assessments
performed on a variety of code constructs using entities to which assertion
pragmas or aspects apply, disabled by the appropriate Assertion_Policy
directive.

We also verify that no decision coverage violation is emitted for the disabled
assertions.

.. qmlink:: TCIndexImporter

   *

