module xcov/models/evaluations

--  This module gives way to represent the evaluation of a decision; that
--  is to say, to attach a value (True, False, Unknown) to each condition
--  of a decision. The structure that ties each condition of a decision to
--  its value for a given evaluation is called an evaluation vector.

private open xcov/models/decision_types
private open util/relation

abstract sig Tristate {}

one sig T_True extends Tristate {}
one sig T_False extends Tristate {}
one sig T_Unknown extends Tristate {}

one sig Tristate_Conversions
{
   --  conversion table from Tristate to Decision_Outcome

   to_outcome_mapping : Tristate one -> lone Decision_Outcome
}
{
   no to_outcome_mapping [T_Unknown]
   to_outcome_mapping [T_True] = Outcome_True
   to_outcome_mapping [T_False] = Outcome_False
}

----------------
-- to_outcome --
----------------

fun to_outcome [v : Tristate] : Decision_Outcome
{
   --  Convert a Tristate into a Decision_Outcome

   v.(Tristate_Conversions.to_outcome_mapping)
}

--------------------------
-- is_evaluation_vector --
--------------------------

pred is_evaluation_vector [values : Condition -> Tristate]
{
   --  True if values is a valid evaluation vector

   --  Partial function
   all n : dom [values] | lone n.values
}

pred is_evaluation_vector
[values : Condition -> Tristate,
domain  : set Condition]
{
   --  True if values is a valid evaluation vector which specifies
   --  all values of conditions on the given domain

   --  Total function
   is_evaluation_vector [values]
   dom [values] = domain
}

----------------------------
-- evaluation_vector_pair --
----------------------------

pred is_evaluation_vector_pair [ev1, ev2 : Condition -> Tristate]
{
   --  True if ev1 and ev2 are two evaluation vectors that are
   --  valid for the same decision

   is_evaluation_vector [ev1]
   is_evaluation_vector [ev2]
   dom [ev1] = dom [ev2]
}

---------------------
-- cond_both_taken --
---------------------

pred cond_both_taken [n : Condition, evt, evf : Condition -> Tristate]
{
   --  True if n has taken both values: True in evt, false in evf

   is_evaluation_vector_pair [evt, evf]
   n.evt = T_True and n.evf = T_False
}

-----------------------------
-- cond_independent_change --
-----------------------------

pred cond_independent_change [n : Condition, evt, evf : Condition -> Tristate]
{
   --  True if n has taken both values (True and False) in evt and evf
   --  and if only n's value have changed between evt and evf

   is_evaluation_vector_pair [evt, evf]

   cond_both_taken [n, evt, evf]
   all other : dom [evt] - n |
      not (T_True + T_False) in other.(evt + evf)
}
