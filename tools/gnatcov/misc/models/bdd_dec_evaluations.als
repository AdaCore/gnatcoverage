module xcov/models/bdd_dec_evaluations

--  This module represents a proof of the equivalence of evaluating a
--  decision and going through its BDD.

private open xcov/models/decision_types
private open xcov/models/evaluations
private open xcov/models/bdd_decs
private open xcov/models/bdd_evaluations
private open xcov/models/decision_evaluations

private open util/relation
private open util/ternary as t

--  The property that this modules aims to prove is the equivalence
--  of decision evaluation and BDD paths. This property could be
--  expressed by the follow assertions:

assert bdd_path_implies_decision_eval {
   all db : Dec_BDD |
      all values : Condition -> Tristate |
         all path : Condition -> (Condition + Decision_Outcome) |
            some result : Decision_Element -> Tristate |
               let root = root [db.decision] {
                  is_bdd_evaluation [db.bdd.if_true, db.bdd.if_false,
                  path, values, to_outcome [root.result]] implies
                  eval_decision [db.decision, values, result]
               }
}

assert decision_eval_implies_bdd_path {
   all db : Dec_BDD |
      all values : Condition -> Tristate |
         all result : Decision_Element -> Tristate |
            some path : Condition -> (Condition + Decision_Outcome) |
               let root = root [db.decision] {
                  eval_decision [db.decision, values, result] implies
                  is_bdd_evaluation [db.bdd.if_true, db.bdd.if_false,
                  path, values, to_outcome [root.result]]
               }
}

--  ...however, these assertion cannot be checked by Alloy's analyzer, because
--  of their existencial quantifications on a relation.
--
--  The only way out is to give a constructive proof. We need to build
--  together the decision evaluation (result : Decision_Element -> Tristate)
--  and the bdd path (path : Condition -> (Condition + Decision_Outcome)).
--  We would then show that what we have constructed are *indeed*
--  a decision evaluation and a bdd path, and that they are consistent.

--------------------
-- eval_condition --
--------------------

pred eval_condition
[c     : Condition,
values : Condition -> Tristate,
result : Tristate,
path   : Condition -> (Condition + Decision_Outcome)]
{
   --  Assuming that c has been evaluated (i.e. is not T_Unknown in values),
   --  set path to the edge that has been followed the corresponding bdd
   --  of a simple condition decision.

   eval_condition [c, values, result]
   result = T_True implies path = (c -> Outcome_True)
   result = T_False implies path = (c -> Outcome_False)
}

---------------------------
-- unevaluated_condition --
---------------------------

pred unevaluated_condition
[c     : Condition,
values : Condition -> Tristate,
result : Tristate,
path   : Condition -> (Condition + Decision_Outcome)]
{
   --  Assuming that c has not been evaluated in values, return a null path

   unevaluated_condition [c, values, result]
   no path
}

--------------
-- eval_not --
--------------

pred eval_not
[op_value : Tristate,
result    : Tristate,
op_path   : Condition -> (Condition + Decision_Outcome),
path      : Condition -> (Condition + Decision_Outcome)]
{
   --  * Considering a decision D that has been evaluated;
   --  * op_path being the path through its BDD corresponding to
   --    this evaluation;
   --  * op_value being the result of this evaluation;
   --
   --  return the result of the evaluation of "not D", and the path
   --  through "not D"'s BDD.

   -------------------
   -- preconditions --
   -------------------

   is_sequence [op_path]

   --------------------
   -- postconditions --
   --------------------

   eval_not [op_value, result]

   let penultimate_node = op_path.Decision_Outcome |
      let outcome_r = penultimate_node.op_path |
         let outcome = Decision_Outcome - outcome_r
      {
         path = (op_path
                 - (penultimate_node -> outcome_r)
                 + (penultimate_node -> outcome))
      }
}

---------------------
-- unevaluated_not --
---------------------

pred unevaluated_not
[op_value : Tristate,
result    : Tristate,
op_path   : Condition -> (Condition + Decision_Outcome),
path      : Condition -> (Condition + Decision_Outcome)]
{
   --  Assuming that "not D" has not been evaluated, return a null path

   unevaluated_not [op_value, result]
   no op_path
   no path
}

---------------------------------
-- eval_short_circuit_operator --
---------------------------------

pred eval_short_circuit_operator
[SC          : Tristate,
result_left  : Tristate,
result_right : Tristate,
result       : Tristate,
path_l       : Condition -> (Condition + Decision_Outcome),
path_r       : Condition -> (Condition + Decision_Outcome),
path         : Condition -> (Condition + Decision_Outcome)]
{
   --  * Considering a decision DL (resp. DR) that has been evaluated;
   --  * path_l (resp. path_r) being the path through its BDD corresponding to
   --    this evaluation;
   --  * result_left (resp. result_right) being the result of this evaluation;
   --
   --  and "op" being "and then" if SC = T_False, "or else" if SC = T_True,x
   --  return the result of the evaluation of "DL op DR", and the path
   --  through "DL op DR"'s BDD.

   -------------------
   -- preconditions --
   -------------------

   is_sequence [path_l]
   is_sequence [path_r]

   --------------------
   -- postconditions --
   --------------------

   eval_short_circuit_operator [SC, result_left, result_right, result]
   result_left = SC implies path = path_l
   result_left != SC implies concat [path, path_l, path_r]
}

-------------------
-- eval_and_then --
-------------------

pred eval_and_then
[result_left : Tristate,
result_right : Tristate,
result       : Tristate,
path_l       : Condition -> (Condition + Decision_Outcome),
path_r       : Condition -> (Condition + Decision_Outcome),
path         : Condition -> (Condition + Decision_Outcome)]
{
   eval_short_circuit_operator [T_False, result_left, result_right, result,
                                path_l, path_r, path]
}

------------------
-- eval_or_else --
------------------

pred eval_or_else
[result_left : Tristate,
result_right : Tristate,
result       : Tristate,
path_l       : Condition -> (Condition + Decision_Outcome),
path_r       : Condition -> (Condition + Decision_Outcome),
path         : Condition -> (Condition + Decision_Outcome)]
{
   eval_short_circuit_operator [T_True, result_left, result_right, result,
                                path_l, path_r, path]
}

----------------------------------------
-- unevaluated_short_circuit_operator --
----------------------------------------

pred unevaluated_short_circuit_operator
[result_left : Tristate,
result_right : Tristate,
result       : Tristate,
path_l       : Condition -> (Condition + Decision_Outcome),
path_r       : Condition -> (Condition + Decision_Outcome),
path         : Condition -> (Condition + Decision_Outcome)]
{
   --  Assuming that "DL op DR" has not been evaluated, return a null path

   unevaluated_short_circuit_operator [result, result_left, result_right]
   no path_l
   no path_r
   no path
}

------------------
-- eval_dec_bdd --
------------------

pred eval_dec_bdd
[db    : Dec_BDD,
values : Condition -> Tristate,
result : Decision_Element -> Tristate,
paths  : Decision_Element -> (Condition -> (Condition + Decision_Outcome))]
{
   --  Assuming that the given condition values is a consistent evaluation
   --  of d, paths relates each decision element to a path in the bdd of
   --  the corresponding sub-decision; so that each path is consistent
   --  with the given condition values.

   -------------------
   -- preconditions --
   -------------------

   is_evaluation_vector [values]
   dom [values] = conditions [db.decision]
   dom [result] = elements [db.decision]

   --------------------
   -- postconditions --
   --------------------

   root [db.decision].result != T_Unknown

   all c : conditions [db.decision] {
      c.result = c.values
      unevaluated_condition [c, values, c.result, c.paths]
         or eval_condition [c, values, c.result, c.paths]
   }

   all op : not_ops [db.decision] |
      let r = op.(db.decision.child_un) {
         unevaluated_not [r.result, op.result, r.paths, op.paths]
         or eval_not [r.result, op.result, r.paths, op.paths]
      }

   all op : and_then_ops [db.decision] |
      let l = op.(db.decision.child_bin_left) |
         let r = op.(db.decision.child_bin_right) {
            unevaluated_short_circuit_operator [l.result, r.result, op.result,
                                                l.paths, r.paths, op.paths]
            or eval_and_then [l.result, r.result, op.result,
                              l.paths, r.paths, op.paths]
         }

   all op : or_else_ops [db.decision] |
      let l = op.(db.decision.child_bin_left) |
         let r = op.(db.decision.child_bin_right) {
            unevaluated_short_circuit_operator [l.result, r.result, op.result,
                                                l.paths, r.paths, op.paths]
            or eval_or_else [l.result, r.result, op.result,
                             l.paths, r.paths, op.paths]
         }
}

-------------------------------------------------------------------------------
--  private part

assert evals_are_paths {
   --  Assert that the build procedure modeled by eval_dec_bdd does build
   --  a valid decision evaluation and a valid bdd evaluation, and that
   --  they are consistent.

   all db : Dec_BDD |
   all values : Condition -> Tristate |
   all result : Decision_Element -> Tristate |
   all paths :
   Decision_Element -> (Condition -> (Condition + Decision_Outcome)) {
      eval_dec_bdd [db, values, result, paths] implies
      let root = root [db.decision] |
         let path = root.paths
         {
            --  The path that we built during evaluation is indeed a
            --  path in the decision's bdd for these condition values,
            --  reaching the same outcome.
            is_bdd_evaluation [db.bdd.if_true, db.bdd.if_false, path, values,
                              to_outcome [root.result]]
         }
   }
}

check evals_are_paths for 7 but 1 Decision, 1 BDD

private pred show_condition_eval
[db    : Dec_BDD,
values : Condition -> Tristate,
result : Decision_Element -> Tristate,
paths  : Decision_Element -> (Condition -> (Condition + Decision_Outcome))]
{
   --  Show the evaluation of a simple condition decision

   #conditions [db.decision] = 1
   #not_ops [db.decision] = 0
   eval_dec_bdd [db, values, result, paths]
}

run show_condition_eval for 7 but 1 BDD, 1 Decision

private pred show_nots_eval
[db    : Dec_BDD,
values : Condition -> Tristate,
result : Decision_Element -> Tristate,
paths  : Decision_Element -> (Condition -> (Condition + Decision_Outcome)),
path   : Condition -> (Condition + Decision_Outcome)]
{
   --  Show the evaluation of a decision that contains only "not" operators
   --  and only one condition

   #conditions [db.decision] = 1
   #not_ops [db.decision] > 0
   path = root [db.decision].paths
   eval_dec_bdd [db, values, result, paths]
}

run show_nots_eval for 7 but 1 BDD, 1 Decision

private pred show_and_then_eval
[db    : Dec_BDD,
values : Condition -> Tristate,
result : Decision_Element -> Tristate,
paths  : Decision_Element -> (Condition -> (Condition + Decision_Outcome)),
path   : Condition -> (Condition + Decision_Outcome)]
{
   --  Show the evaluation of a decision of the form "C1 and then C2"

   #and_then_ops [db.decision] = 1
   #or_else_ops [db.decision] = 0
   #not_ops [db.decision] = 0
   path = root [db.decision].paths
   eval_dec_bdd [db, values, result, paths]
}

run show_and_then_eval for 7 but 1 BDD, 1 Decision

private pred show_and_then_eval_shortcut
[db    : Dec_BDD,
values : Condition -> Tristate,
result : Decision_Element -> Tristate,
paths  : Decision_Element -> (Condition -> (Condition + Decision_Outcome)),
path   : Condition -> (Condition + Decision_Outcome)]
{
   --  Show the evaluation of a decision of the form "C1 or else C2"

   #and_then_ops [db.decision] = 1
   #or_else_ops [db.decision] = 0
   #not_ops [db.decision] = 0
   path = root [db.decision].paths
   #path = 1
   eval_dec_bdd [db, values, result, paths]
}

run show_and_then_eval_shortcut for 7 but 1 BDD, 1 Decision
