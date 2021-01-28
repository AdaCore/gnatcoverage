module xcov/models/decision_evaluations

--  This module provides a model for the evaluation of a decision using the
--  following recursive procedure:
--
--  Eval.Condition
--    The value of a decision that consists in a lone condition is the
--    value of the condition.
--
--  Eval.Not
--    To evaluate "not (D)", evaluate D and take the opposite value
--
--  Eval.Short_Circuit_Operator
--    To evaluate "(D1) op (D2)", evaluate D1. If D1 = SC then the
--    value is SC, else evaluate D2, and the value is that of D2.

private open xcov/models/decision_types
private open xcov/models/decisions
private open xcov/models/evaluations
private open util/relation

--------------------
-- eval_condition --
--------------------

pred eval_condition
[c     : Condition,
values : Condition -> Tristate,
result : Tristate]
{
   --  Assuming that c has been evaluated (i.e. is not T_Unknown in values),
   --  set result to this value

   -------------------
   -- preconditions --
   -------------------

   c in dom [values]
   c.values != T_Unknown
   is_evaluation_vector [values]

   --------------------
   -- postconditions --
   --------------------

   result = c.values
}

---------------------------
-- unevaluated_condition --
---------------------------

pred unevaluated_condition
[c     : Condition,
values : Condition -> Tristate,
result : Tristate]
{
   --  True if c has not been evaluated in values

   c in dom [values]
   is_evaluation_vector [values]

   c.values = T_Unknown
   result = T_Unknown
}

--------------
-- eval_not --
--------------

pred eval_not
[op_value : Tristate,
result    : Tristate]
{
   --  op_value being an evaluation of "D", let result be an
   --  evaluation of "not (D)".

   ------------------
   -- precondition --
   ------------------

   op_value != T_Unknown

   --------------------
   -- postconditions --
   --------------------

   result = (Tristate - T_Unknown) - op_value
}

---------------------
-- unevaluated_not --
---------------------

pred unevaluated_not
[op_value : Tristate,
result    : Tristate]
{
   --  op_value being the value of a non-evaluated decision
   --  "D" (i.e. T_Unknown), let result be the value of "not (D)"
   --  (i.e. T_Unknown as well).

   op_value = T_Unknown
   result = T_Unknown
}

-------------------
-- eval_and_then --
-------------------

pred eval_and_then
[result_left : Tristate,
result_right : Tristate,
result       : Tristate]
{
   eval_short_circuit_operator [T_False, result_left, result_right, result]
}

------------------
-- eval_or_else --
------------------

pred eval_or_else
[result_left : Tristate,
result_right : Tristate,
result       : Tristate]
{
   eval_short_circuit_operator [T_True, result_left, result_right, result]
}

---------------------------------
-- eval_short_circuit_operator --
---------------------------------

pred eval_short_circuit_operator
[SC          : Tristate,
result_left  : Tristate,
result_right : Tristate,
result       : Tristate]
{
   --  result_left being the result of the evaluation of DL,
   --  result_right being the result of the evaluation of DR,
   --  let result be the evaluation of:
   --  (1) "DL and then DR" if SC = T_False;
   --  (2) "DL or else DR" if SC = T_True.

   -------------------
   -- preconditions --
   -------------------

   SC != T_Unknown
   result_left != T_Unknown

   --------------------
   -- postconditions --
   --------------------
   result_left = SC implies
      (result = SC and result_right = T_Unknown)
   result_left != SC implies
      (result = result_right and result_right != T_Unknown)
}

----------------------------------------
-- unevaluated_short_circuit_operator --
----------------------------------------

pred unevaluated_short_circuit_operator
[result_left : Tristate,
result_right : Tristate,
result       : Tristate]
{
   --  result_left and result_right being the value of two non-evaluated
   --  decision DL and DR (i.e. T_Unknown), let result be the value of
   --  "DL op DR" (i.e. T_Unknown as well), "op" being any short-circuit
   --  operator.

   result_left = T_Unknown
   result_right = T_Unknown
   result = T_Unknown
}

----------------
-- evaluation --
----------------

pred evaluation
[d : Decision,
e  : Decision_Element -> Tristate]
{
   --  True if e is a consistent evaluation of d, i.e. if it
   --  relates each decision element to the value of its sub-decision
   --  for a valid set of condition values.

   let values = e & (conditions[d] -> Tristate) {
      eval_decision [d, values, e]
   }
}

-------------------
-- eval_decision --
-------------------

pred eval_decision
[d     : Decision,
values : Condition -> Tristate,
result : Decision_Element -> Tristate]
{
   --  True if the given condition values is a consistent evaluation of d.
   --  result relates each decision element to the value of its sub-decision
   --  for the given condition values.

   -------------------
   -- preconditions --
   -------------------

   is_evaluation_vector [values]
   conditions [d] = dom [values]

   --------------------
   -- postconditions --
   --------------------

   root [d].result != T_Unknown

   all c : conditions [d] {
      c.result = c.values
      unevaluated_condition [c, values, c.result]
         or eval_condition [c, values, c.result]
   }

   all op : not_ops [d] |
      let r = op.(d.child_un) {
         unevaluated_not [r.result, op.result]
         or eval_not [r.result, op.result]
      }

   all op : and_then_ops [d] |
      let l = op.(d.child_bin_left) |
         let r = op.(d.child_bin_right) {
            unevaluated_short_circuit_operator [l.result, r.result, op.result]
            or eval_and_then [l.result, r.result, op.result]
         }

   all op : or_else_ops [d] |
      let l = op.(d.child_bin_left) |
         let r = op.(d.child_bin_right) {
            unevaluated_short_circuit_operator [l.result, r.result, op.result]
            or eval_or_else [l.result, r.result, op.result]
         }
}

-------------------------------------------------------------------------------
--  private part

--------------------------
-- assertions and tests --
--------------------------

private pred show_nots_eval
[d     : Decision,
values : Condition -> Tristate,
result : Decision_Element -> Tristate]
{
   --  Show the evaluation of a decision that contains only "not" operators
   --  and only one condition

   #conditions [d] = 1
   eval_decision [d, values, result]
}

run show_nots_eval for 7 but 1 Decision

private pred show_and_then_eval
[d     : Decision,
values : Condition -> Tristate,
result : Decision_Element -> Tristate]
{
   --  Show the evaluation of a decision of the form "C1 and then C2"

   #conditions [d] = 2
   #and_then_ops [d] = 1
   #not_ops [d] = 0
   eval_decision [d, values, result]
}

run show_and_then_eval for 7 but 1 Decision

private pred show_or_else_eval
[d     : Decision,
values : Condition -> Tristate,
result : Decision_Element -> Tristate]
{
   --  Show the evaluation of a decision of the form "C1 or else C2"

   #conditions [d] = 2
   #or_else_ops [d] = 1
   #not_ops [d] = 0
   eval_decision [d, values, result]
}

run show_or_else_eval for 7 but 1 Decision

private pred show_complex_dec_eval
[d     : Decision,
values : Condition -> Tristate,
result : Decision_Element -> Tristate]
{
   --  Show the evaluation of a decision that contains "and then",
   --  "or else" and "not".

   #and_then_ops [d] > 0
   #or_else_ops [d] > 0
   #not_ops [d] > 0
   eval_decision [d, values, result]
}

run show_complex_dec_eval for 7 but 1 Decision, 18 Decision_Element

