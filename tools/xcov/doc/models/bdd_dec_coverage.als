module xcov/models/bdd_dec_coverage

--  This module checks that the definitions of MC/DC in bdd_coverage
--  and decision_coverage are equivalent

private open xcov/models/bdds
private open xcov/models/bdd_evaluations
private open xcov/models/evaluations
private open xcov/models/bdd_relations[Condition, Decision_Outcome]
private open xcov/models/bdd_coverage as B

private open xcov/models/decision_types
private open xcov/models/decisions
private open xcov/models/evaluations
private open xcov/models/decision_evaluations
private open xcov/models/decision_coverage as D

private open xcov/models/bdd_decs

private open util/relation
private open util/sequniv
private open util/ternary as ter

sig BDD_Dec_Execution extends Execution {
   --  Models a set of evaluations of the same Decision

   db         : Dec_BDD,
   --  The association of the considered decision with its BDD

   full_evals : Int -> (Decision_Element -> Tristate)
   --  Same as Execution.evaluation_vectors, but for all elements
   --  of the decision (not just conditions).
}
{
   --  full_evals are valid evaluations of the decisions are in sync
   --  with the execution outcome
   D/are_evaluation_vectors [db.decision, outcome, full_evals]

   --  BDDs in sync
   bdd = db.bdd

   --  Evals in sync
   evaluation_vectors = (Int-> (Condition -> Tristate)) & full_evals
}

pred show_execution [] {
}

run show_execution for 7
but exactly 3 Condition,
    exactly 0 Not,
    exactly 1 BDD_Dec_Execution,
    exactly 1 Dec_BDD,
    exactly 1 BDD,
    exactly 1 Decision

assert bdd_unique_cause_equals_decision_unique_cause {
   --  Check that the two definition of Unique Cause MC/DC given
   --  in bdd_coverage and decision_coverage are equivalent

   all ex : BDD_Dec_Execution {
      B/unique_cause [ex] <=> D/unique_cause [ex.db.decision,
                                              ex.outcome,
                                              ex.full_evals]
   }
}

check bdd_unique_cause_equals_decision_unique_cause for 5
but 3 Condition, 1 BDD_Dec_Execution, 1 Dec_BDD, 1 BDD, 1 Decision

check bdd_unique_cause_equals_decision_unique_cause for 7
but exactly 3 Condition,
    exactly 0 Not,
    exactly 1 BDD_Dec_Execution,
    exactly 1 Dec_BDD,
    exactly 1 BDD,
    exactly 1 Decision

assert bdd_masking_mcdc_equals_decision_masking_mcdc {
   --  Check that the two definition of Masking MC/DC given
   --  in bdd_coverage and decision_coverage are equivalent

   all ex : BDD_Dec_Execution {
      B/masking_mcdc [ex] <=> D/masking_mcdc [ex.db.decision,
                                              ex.outcome,
                                              ex.full_evals]
   }
}

check bdd_masking_mcdc_equals_decision_masking_mcdc for 5
but 3 Condition, 1 BDD_Dec_Execution, 1 Dec_BDD, 1 BDD, 1 Decision

check bdd_masking_mcdc_equals_decision_masking_mcdc for 8
but exactly 4 Condition,
    exactly 0 Not,
    exactly 1 BDD_Dec_Execution,
    exactly 1 Dec_BDD,
    exactly 1 BDD,
    exactly 1 Decision
