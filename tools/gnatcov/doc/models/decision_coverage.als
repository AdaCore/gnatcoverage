module xcov/models/decision_coverage

--  This module provides a model for coverage criteria defined using
--  the graph-coloring algorithm defined in DOT/FAA/AR-01/18 (Chilenski,
--  "An Investigation of Three Forms of the Modified Condition/Decision
--  Coverage (MCDC) Criterion.
--
--  In order to take advantage of Alloy's facilities to handle relations,
--  this actually defines a sub-graph of the decision syntax tree, called
--  the influence graph, that is such that each edge connects two T_True
--  nodes in the influence tree. That in particular allows an elegant
--  definition of the influence set using the transitive closure.
--  This module also provides a way to build this influence graph without
--  building the intermediate influence tree; an assertion checks that
--  these two definitions of influence graph are equivalent.

private open xcov/models/decision_types
private open xcov/models/decisions
private open xcov/models/evaluations
private open xcov/models/decision_evaluations
private open util/relation
private open util/sequniv
private open util/ternary as ter

------------------
-- xor_tristate --
------------------

fun xor_tristate [s1, s2 : Tristate] : Tristate
{
   --  Extension of xor to three-state boolean algebra as defined in
   --  DOT/FAA/AR-01/18: anything xor T_Unknown = False.

   ((s1 + s2) =  (T_True + T_False)) => T_True else T_False
}

------------------------
-- is_evaluation_pair --
------------------------

pred is_evaluation_pair
[d           : Decision,
eval1, eval2 : Decision_Element -> Tristate]
{
   --  True if eval1 and eval2 are two valid evaluations for d.

   evaluation [d, eval1]
   evaluation [d, eval2]
}

----------------------
-- elements_changed --
----------------------

fun elements_changed
[eval1, eval2 : Decision_Element -> Tristate]
: Decision_Element
{
   --  Return The set of decision elements that have been evaluated
   --  in both eval1 and eval2 and whose values are different between
   --  these two evaluations. In other words, it keeps all conditions c
   --  such that xor_tristate [c.eval1, c.eval2] = T_True.

   (eval1.T_True & eval2.T_False) + (eval1.T_False & eval2.T_True)
}
-----------------------
-- is_influence_tree --
-----------------------

pred is_influence_tree
[d     : Decision,
i_tree : Decision_Element -> Tristate]
{
   --  True if i_tree is a valid influence tree for d.

   dom [i_tree] = elements [d]
   ran [i_tree] = T_True + T_False
}

--------------------
-- influence_tree --
--------------------

pred influence_tree
[d           : Decision,
result       : Decision_Element -> Tristate,
eval1, eval2 : Decision_Element -> Tristate]
{
   --  eval1 and eval2 being two consistent evaluations of decision d,
   --  result is the Influence tree of two evaluations of the
   --  same decision, as defined in DOT/FAA/AR-01/18:
   --
   --  * for each evaluations, the decision syntax tree is colored with
   --  the evaluation of each node (T_True, T_False, T_Unknown);
   --
   --  * the influence tree is then obtained by applying xor_tristate on
   --  the two colored syntax trees.

   -------------------
   -- preconditions --
   -------------------

   is_evaluation_pair [d, eval1, eval2]
   is_influence_tree [d, result]

   --------------------
   -- postconditions --
   --------------------

   all e : elements [d] |
      result [e] = xor_tristate [eval1 [e], eval2 [e]]
}

-------------------------------
-- influence_graph_from_tree --
-------------------------------

fun influence_graph_from_tree
[d      : Decision,
i_tree  : Decision_Element -> Tristate]
: Decision_Element -> Decision_Element
{
   --  Build the influence graph, i.e. a sub-graph of the
   --  decision graph such that two nodes are connected iff
   --  they are both colored with T_True in the influence tree.
   --  Not quite a sub-graph, actually, as the influence graph
   --  is reflexive (a condition *does* influence itself) whereas
   --  the decision graph is not (it is a syntax tree). Reflexivity
   --  is needed here to handle properly the case of a simple condition
   --  decision, and also to make sure that the T_True color information
   --  would not be lost even if it has no T_True in its immediate
   --  neighbourhood.

   let rel_changed = (i_tree.T_True -> i_tree.T_True) |
      (rel_changed & graph [d]
       + rel_changed & iden)
}

---------------------
-- influence_graph --
---------------------

fun influence_graph
[d           : Decision,
eval1, eval2 : Decision_Element -> Tristate]
: Decision_Element -> Decision_Element
{
   --  Assuming that eval1 and eval2 are two valid evaluations of
   --  d, return the graph of their influence tree. Same as
   --  influence_graph_from_tree, but without actually computing the
   --  influence tree.

   let changed = elements_changed [eval1, eval2] |
      let rel_changed =  changed -> changed |
	 (rel_changed  & graph [d]
	  + rel_changed & iden)
}

-------------------
-- influence_set --
-------------------

fun influence_set
[d      : Decision,
i_graph : Decision_Element -> Decision_Element] : set Condition
{
   --  Given a decision d and a valid influence graph for this decision,
   --  return the influence set.

   root [d].((root[d] -> Condition) & ^i_graph)
}

----------------------------
-- are_evaluation_vectors --
----------------------------

pred are_evaluation_vectors
[d      : Decision,
outcome : seq Decision_Outcome,
evals   : Int -> (Decision_Element -> Tristate)]
{
   --  True if evals and outcome are valid evaluation vectors for d.

   ter/dom [evals] = inds [outcome]
   all i : inds [outcome] {
      evaluation [d, evals [i]]
      outcome [i] = to_outcome [(root [d]).(evals [i])]
   }
}

-------------------------------------
-- cond_independent_effect_masking --
-------------------------------------

pred cond_independent_effect_masking
[d      : Decision,
c       : Condition,
outcome : seq Decision_Outcome,
evals   : Int -> (Decision_Element -> Tristate)]
{
   --  True if in evals there is an evaluation pair that allows to reach
   --  Masking MC/DC for condition c in decision d.

   -------------------
   -- preconditions --
   -------------------

   c in conditions [d]
   are_evaluation_vectors [d, outcome, evals]

   --------------------
   -- postconditions --
   --------------------

   some e1, e2 : inds [outcome] |
      let g = influence_graph [d, evals [e1], evals [e2]] |
	 influence_set [d, g] = c
}

-----------------------------
-- cond_independent_effect --
-----------------------------

pred cond_independent_effect
[d      : Decision,
c       : Condition,
outcome : seq Decision_Outcome,
evals   : Int -> (Decision_Element -> Tristate)]
{
   --  True if in evals there is an evaluation pair that allows to reach
   --  Unique Cause MC/DC for condition c in decision d.

   -------------------
   -- preconditions --
   -------------------

   c in conditions [d]
   are_evaluation_vectors [d, outcome, evals]

   --------------------
   -- postconditions --
   --------------------

   some e1, e2 : inds [outcome] |
      let g = influence_graph [d, evals [e1], evals [e2]] {
	 influence_set [d, g] = c
	 ran [g] & Condition = c
      }
}

------------------
-- masking_mcdc --
------------------

pred masking_mcdc
[d      : Decision,
outcome : seq Decision_Outcome,
evals   : Int -> (Decision_Element -> Tristate)]
{
   --  True if evals allows to reach Masking MC/DC for decision d.

   all c : conditions [d] |
      cond_independent_effect_masking [d, c, outcome, evals]
}

------------------
-- unique_cause --
------------------

pred unique_cause
[d      : Decision,
outcome : seq Decision_Outcome,
evals   : Int -> (Decision_Element -> Tristate)]
{
   --  True if evals allows to reach Unique Cause MC/DC for decision d.

   all c : conditions [d] |
      cond_independent_effect [d, c, outcome, evals]
}

-------------------------------------------------------------------------------
--  private part

assert influence_graphs_equivalence {
   --  Check that building the influence graph from the evaluation pair
   --  or from the influence tree will give the same result.

   all d : Decision |
      all eval1, eval2 : elements[d] -> Tristate |
	 all i_tree : elements[d] -> Tristate {
	    (influence_tree [d, i_tree, eval1, eval2]
	     implies (influence_graph [d, eval1, eval2]
		      = influence_graph_from_tree [d, i_tree]))
	 }
}

check influence_graphs_equivalence for 5 but 1 Decision, 8 Decision_Element

assert unique_cause_stronger_than_masking {
   --  Check that Unique Cause MC/DC implies Masking MC/DC.

   all d : Decision |
      all outcome : seq Decision_Outcome |
	 all evals : Int -> (Decision_Element -> Tristate) |
	    (unique_cause [d, outcome, evals]
	     implies masking_mcdc [d, outcome, evals])
}

check unique_cause_stronger_than_masking for 5 but 1 Decision,
7 Decision_Element

private pred complex_decision [d : Decision]
{
   #and_then_ops [d] > 0
   #or_else_ops [d] > 0
   #not_ops [d] > 0
}

private pred show_complex_influence_tree
[d               : Decision,
result           : Decision_Element -> Tristate,
eval1,   eval2   : Decision_Element -> Tristate]
{
   --  Show an influence tree in a decision that contains "and then",
   --  "or else" and "not".

   complex_decision [d]

   result [root [d]] = T_True
   influence_tree [d, result, eval1, eval2]
}

run show_complex_influence_tree for 5 but 1 Decision, 8 Decision_Element

private pred show_complex_influence_graph
[d               : Decision,
i_graph          : Decision_Element -> Decision_Element,
eval1,   eval2   : Decision_Element -> Tristate]
{
   --  Show an influence graph in a decision that contains "and then",
   --  "or else" and "not".

   complex_decision [d]

   is_evaluation_pair [d, eval1, eval2]
   i_graph = influence_graph [d, eval1, eval2]
}

run show_complex_influence_graph for 5 but 1 Decision, 8 Decision_Element

private pred show_cond_independent_effect_masking
[d      : Decision,
c       : Condition,
outcome : seq Decision_Outcome,
evals   : Int -> (Decision_Element -> Tristate)]
{
   #inds [outcome] = 2
   complex_decision [d]
   cond_independent_effect_masking [d, c, outcome, evals]
}

run show_cond_independent_effect_masking for 5 but 1 Decision,
8 Decision_Element

private pred show_masking_mcdc
[d      : Decision,
outcome : seq Decision_Outcome,
evals   : Int -> (Decision_Element -> Tristate)]
{
   --  Show an execution that allows to reach Masking MC/DC
   --  but not Unique Cause

   no not_ops [d]
   masking_mcdc [d, outcome, evals]
   not unique_cause [d, outcome, evals]
}

run show_masking_mcdc for 5 but 1 Decision, exactly 7 Decision_Element
