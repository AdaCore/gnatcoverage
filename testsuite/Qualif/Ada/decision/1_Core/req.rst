Core DC requirements
====================

Core expectations for Decision Coverage
(DC) assessments. All the other DC-related sections rely on this one.

For Decision Coverage assessments, the tool processes as decisions the
following set of expressions:

* The Boolean guards in *explicit control-flow* language constructs such
  as If statements or While loops,

* The controlling tests of Ada2012 *if-expressions*,

* The Ada2012 *quantified-expressions* predicates.

The types involved need not be restricted to the standard Boolean
type; they may be subtypes or types derived from Boolean.

In this context, the following set of rules shall be obeyed on top of the
requirements governing Statement Coverage:

======  ======================================================================
Rule #  Description
======  ======================================================================
1       When the control-flow statement influenced by a decision has not been
        executed, that statement shall be reported as not covered and nothing
        shall be reported about the decision.

2       When a decision evaluates to only True or only False, not both, it
        shall be reported as only partially covered. In this case as in the
        previous one, the tool shall designate the decision with an unambiguous
        file-name:line#:col# reference.

3       When a decision evaluates to both True and False, no decision coverage
        violation shall be reported for it.

4       When a decision is never evaluated even though the enclosing statement
        has been executed (e.g. because of an exception), the decision shall
        be reported as never evaluated.

5       The tool shall be able to handle arbitrarily complex decisions in any
        controlling context where they might appear.
======  ======================================================================


.. rubric:: Testing Strategy


We validate all the DC rules based on three main subsets of testcases:


.. qmlink:: SubsetIndexImporter

   *



Rules #1 to 3 are validated by variations exercised in every individual
testcase, where we consistently check each decision of interest in multiple
ways, always including:

* a situation where the statements influenced by the decision are not
  executed at all (*rule #1*).

* a set of test vectors where the decision evaluates only True (*rule #2*),

* a set of test vectors where the decision evaluates only False (*rule #2*),

* a set of test vectors where the decision evaluates both True and False
  (*rule #3*),

Rule #4 is validated by the *NoEval* testcase group listed in the table above.
Rule #5 is addressed by the *Topologies* and the *Operands* groups, as implied
by their description.

Ada2012 quantified-expressions require particular care as
their predicates are decisions on their own, nested within the predicated
expression, itself possibly part of a distinct outer decision.

Dedicated testcases in the *Topologies* series verify the proper processing of
various forms of predicates in contexts where the enclosing expression is not
involved in a decision. Some of these testcases exercise predicates consisting
of a single Boolean atom, confirming that any Boolean expression is considered
as a decision in such a context.

Dedicated testcases in the *Operands* series verify the proper processing of
decisions involving predicated expressions as operands, implying the correct
distinction of such decisions and the inner predicates.

We check a mix of FORALL and FORSOME expressions, iterating through a
variety of mechanisms such as array indexes (for all|some I in
Arr'Range) or array items directly (for all|some X of Arr).
