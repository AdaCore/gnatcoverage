module xcov/models/bdd_coverage

--  The module provides a model for some BDD coverage criteria and
--  show how they relates to each other.

private open xcov/models/bdds
private open xcov/models/bdd_evaluations
private open xcov/models/decision_types
private open xcov/models/evaluations
private open xcov/models/bdd_relations[Condition, Decision_Outcome]

private open util/relation
private open util/sequniv
private open util/ternary as ter

sig Execution {
   --  Models a set of evaluations of the same BDD

   bdd : BDD,
   --  The considered BDD

   outcome : seq Decision_Outcome,
   --  This BDD's outcomes for the considered evaluation, recorded in
   --  a sequence.

   --  Note: as each outcome is numbered by a different Int atom,
   --  the number of atoms limits the number of considered evaluations;
   --  therefore, it adds an additional constraint on the number of
   --  of conditions that we can consider to show a given criteria.
   --
   --  To illustrate this limitation, let me take an example.
   --  Suppose that you ask the analyzer to show you a MC/DC
   --  coverage of a BDD in a model with 7 atoms; then,
   --  the maximum number of evaluations that it can show is 7,
   --  and as a consequence the maximum number of conditions in the
   --  considered BDDs is 6.

   paths : Int -> (Condition -> (Condition + Decision_Outcome)),
   --  Set of paths through the BDD for the considered evaluations

   evaluation_vectors : Int -> (Condition -> Tristate)
   --  Set of evaluation vectors for the considered evaluations
}
{
   --  All sequences use the same index range
   ter/dom [evaluation_vectors] = inds [outcome]
   ter/dom [paths] = inds [outcome]

   --  A consistent BDD evaluation for each index
   all i : inds [outcome] |
      is_bdd_evaluation [bdd.if_true, bdd.if_false,
                         paths [i], evaluation_vectors [i], outcome [i]]
}

---------------------
-- cond_both_taken --
---------------------

pred cond_both_taken [ex : Execution, n : Condition]
{
   --  True if n has taken both values (True and False)
   --  in the set of evaluations.

   some t, f : inds [ex.outcome] |
      cond_both_taken [n, ex.evaluation_vectors [t], ex.evaluation_vectors [f]]
}

--------------------------
-- cond_changed_outcome --
--------------------------

pred cond_changed_outcome [ex : Execution, n : Condition]
{
   --  True if n has taken both values (True and False) in the set of
   --  evaluation vectors and if the change in ex's value corresponds to
   --  a change in outcome.

   some t, f : inds [ex.outcome] {
      cond_both_taken [n, ex.evaluation_vectors [t], ex.evaluation_vectors [f]]
      ex.outcome [t] + ex.outcome [f] = (Outcome_True + Outcome_False)
   }
}

-----------------------------
-- cond_independent_effect --
-----------------------------

pred cond_independent_effect [ex : Execution, n : Condition]
{
   --  True if there are two input vectors in the evaluation set so that
   --  n and outcome have taken two different values and which demonstrate
   --  independent effect of n on outcome.

   some t, f : inds [ex.outcome] {
      cond_independent_change [n,
                               ex.evaluation_vectors [t],
                               ex.evaluation_vectors [f]]
      ex.outcome [t] + ex.outcome [f] = (Outcome_True + Outcome_False)
   }
}

-----------------------------
-- cond_independent_effect --
-----------------------------

pred cond_independent_effect_masking [ex : Execution, n : Condition]
{
   --  True if there are two input vectors in the evaluation set so that
   --  n and outcome have taken two different values and which demonstrate
   --  masking independent effect of n on outcome

   some t, f : inds [ex.outcome] {
      --  The outcome changed
      ex.outcome [t] + ex.outcome [f] = (Outcome_True + Outcome_False)

      --  The condition changed
      n.(ex.evaluation_vectors [t]) = T_True
      n.(ex.evaluation_vectors [f]) = T_False

      --  For each vector of the independent pair, changing the
      --  condition value breaks the path to outcome

      let t_switched = sequence_switch_node [ex.paths [t],
                                             ex.bdd.if_false,
                                             n]
      {
            not reachable [t_switched, n, ex.outcome [t]]
      }
      let f_switched = sequence_switch_node [ex.paths [f],
                                             ex.bdd.if_true,
                                             n]
      {
            not reachable [f_switched, n, ex.outcome [f]]
      }
   }
}

------------------
-- unique_cause --
------------------

pred unique_cause [ex : Execution]
{
   --  True if ex is a Unique Cause MC/DC coverage of the bdd

   all n : ex.bdd.nodes | cond_independent_effect [ex, n]
}

------------------
-- masking_mcdc --
------------------

pred masking_mcdc [ex : Execution]
{
   --  True if ex is a Masking MC/DC coverage of the bdd

   all n : ex.bdd.nodes | cond_independent_effect_masking [ex, n]
}

---------------------
-- branch_coverage --
---------------------

pred branch_coverage [ex : Execution]
{
   --  True if ex is a branch coverage of the bdd

   Int.(ex.paths) = (ex.bdd).(if_false + if_true)
}

-------------------------------------------------------------------------------
--  Start of private part

run cond_both_taken for 7 but 1 BDD, 1 Execution
run cond_changed_outcome for 7 but 1 BDD, 1 Execution
run cond_independent_effect for 7 but 1 BDD, 1 Execution

private pred show_unique_cause [ex : Execution]
{
   --  Show Unique Cause MC/DC coverage on a "significantly complicated" bdd
   --  (more than 1 node...)
   --  This predicates also allows to check that a scope with 7 atoms
   --  allows to show the MC/DC coverage of a BDD with 6 nodes.

   #ex.bdd.nodes = 6
   unique_cause [ex]
}

run show_unique_cause for 7 but 1 BDD, 1 Execution

private pred show_masking_mcdc [ex : Execution]
{
   --  Show an execution that allows to reach Masking MC/DC
   --  but not Unique Cause

   masking_mcdc [ex]
   not unique_cause [ex]
}

run show_masking_mcdc for 5 but 1 BDD, 1 Execution

private pred show_branch_coverage [ex : Execution]
{
   --  Show branch coverage on a "significantly complicated" bdd
   --  (more than 1 node...)
   --  This predicates also allows to check that a scope with 7 atoms
   --  allows to show the branch coverage of a BDD with 6 nodes.

   #ex.bdd.nodes = 6
   branch_coverage [ex]
}

run show_branch_coverage for 7 but 1 BDD, 1 Execution

assert unique_cause_implies_branch_coverage {
   --  Assert that Unique Cause MC/DC implies branch coverage

   all ex : Execution |
      unique_cause [ex] implies branch_coverage [ex]
}

check unique_cause_implies_branch_coverage for 7 but 1 BDD, 1 Execution

assert unique_cause_implies_masking_mcdc {
   --  Assert that Unique Cause MC/DC implies Masking MC/DC

   all ex : Execution |
      unique_cause [ex] implies masking_mcdc [ex]
}

check unique_cause_implies_masking_mcdc for 5 but 1 BDD, 1 Execution

assert masking_mcdc_implies_branch_coverage {
   --  Assert that Unique Cause MC/DC implies branch coverage

   all ex : Execution |
      masking_mcdc [ex] implies branch_coverage [ex]
}

check masking_mcdc_implies_branch_coverage for 7 but 1 BDD, 1 Execution

assert path_coverage_implies_unique_cause {
   --  Assert that branch coverage + no diamond implies Unique Cause

   all ex : Execution {
      (not has_diamond [ex.bdd] and branch_coverage [ex])
         implies unique_cause [ex]
   }
}

check path_coverage_implies_unique_cause for 7 but 1 BDD, 1 Execution

private pred branch_coverage_and_not_unique_cause [ex : Execution]
{
   --  True if ex covers bdd's branches, but does not demonstrate Unique Cause

   branch_coverage [ex]
   not unique_cause [ex]
}

run branch_coverage_and_not_unique_cause for 5 but 1 BDD, 1 Execution

assert branch_coverage_and_not_unique_cause_implies_diamond {
   --  Assert that branch coverage + not Unique Cause implies that one
   --  node of the bdd has two fathers. This proves that branch
   --  coverage and MC/DC coverage are not equivalent only when
   --  the bdd has "diamonds".

   all ex : Execution {
      branch_coverage [ex] and not unique_cause [ex] implies
         has_diamond [ex.bdd]
   }
}

check branch_coverage_and_not_unique_cause_implies_diamond for 7
but 1 BDD, 1 Execution

assert path_coverage_implies_masking_mcdc {
   --  Assert that branch coverage + no diamond implies Masking MC/DC

   all ex : Execution {
      (not has_diamond [ex.bdd] and branch_coverage [ex])
         implies masking_mcdc [ex]
   }
}

check path_coverage_implies_masking_mcdc for 6 but 1 BDD, 1 Execution

private pred branch_coverage_and_not_masking_mcdc [ex : Execution]
{
   --  True if ex covers bdd's branches, but does not demonstrate Masking MC/DC

   branch_coverage [ex]
   not masking_mcdc [ex]
}

run branch_coverage_and_not_masking_mcdc for 6 but 1 BDD, 1 Execution

assert branch_coverage_and_not_masking_mcdc_implies_diamond {
   --  Assert that branch coverage + not Masking MC/DC implies that
   --  one node of the bdd has two fathers. This proves that branch
   --  coverage and MC/DC coverage are not equivalent only when
   --  the bdd has "diamonds".

   all ex : Execution {
      branch_coverage [ex] and not masking_mcdc [ex] implies
         has_diamond [ex.bdd]
   }
}

check branch_coverage_and_not_masking_mcdc_implies_diamond for 6
but 1 BDD, 1 Execution
