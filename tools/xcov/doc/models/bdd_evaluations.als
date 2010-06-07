module xcov/models/bdd_evaluations

--  This module provides a model for evaluations of a BDD; it relates
--  path through a BDD to the values taken by its conditions.

private open xcov/models/decision_types
private open xcov/models/evaluations
private open xcov/models/bdd_relations[Condition, Decision_Outcome]

private open util/relation

-----------------------
-- is_bdd_evaluation --
-----------------------

pred is_bdd_evaluation
[if_true, if_false : Condition -> (Condition + Decision_Outcome),
path               : Condition -> (Condition + Decision_Outcome),
values             : Condition -> Tristate,
outcome            : Decision_Outcome]
{
   --  True if the given path if a valid path through (if_true, if_false)
   --  for the given condition values.

   is_path [if_true, if_false, path]
   is_evaluation_vector [values, dom [if_true + if_false]]

   all n : values.T_True    | n.if_true = n.path
   all n : values.T_False   | n.if_false = n.path
   all n : values.T_Unknown | not (n in dom [path])

   outcome = Decision_Outcome <: ran [path]
}

-------------------------------------------------------------------------------
--  private part

--------------------------
-- assertions and tests --
--------------------------

pred show_evaluation
[if_true, if_false : Condition -> (Condition + Decision_Outcome),
path               : Condition -> (Condition + Decision_Outcome),
values             : Condition -> Tristate,
outcome            : Decision_Outcome]
{
   --  Show one evaluation of a bdd

   #if_true > 1
   #if_false > 1
   is_bdd_evaluation [if_true, if_false, path, values, outcome]
}

run show_evaluation for 5
