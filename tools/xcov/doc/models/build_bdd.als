module xcov/models/build_bdd

--  This module provides a model of recursive procedure to build a bdd
--  from decision atoms (conditions and short-circuit operators)

--  The BDD we associate with a decision is constructed using the following
--  recursive procedure:
--
--  build_bdd_condition
--    The BDD for a decision consisting in a single condition C has the node
--    "test C" as its entry point, the label TRUE is assigned to the branch
--    corresponding to "C is TRUE", and the label FALSE is assigned to the
--    branch corresponding to "C is FALSE".
--
--  build_bdd_not
--    The BDD for "not (D)" is the BDD for D where the labels of the exit
--    edges have been swapped.
--
--  build_bdd_short_circuit_operator
--    This rule defines how the BDD for "(D1) op (D2)" is constructed for
--    any short-circuit operator "op".
--
--    If OP is AND THEN, let SC be FALSE
--    If OP is OR ELSE, let SC be TRUE
--
--    Let B1 be the BDD for D1, and B2 the BDD for D2.
--
--    Then B, the BDD for D is obtained by combining B1 and B2 as follows:
--      the entry point is that of B1
--      the exit edge labeled SC of B1 is an exit edge labeled SC of B
--      the other exit edge of B1 connects to the entry point of B2
--      the exit edges of B2 are exit edges of B with the same labels
--
--  Note: this process is explicitly what is implemented in package
--  SC_Obligations.BDD in Xcov.


open xcov/models/bdd_relations[Condition, Decision_Outcome] as B
private open xcov/models/decision_types

-------------------------
-- build_bdd_condition --
-------------------------

pred build_bdd_condition
[if_true, if_false : Condition -> (Condition + Decision_Outcome),
c                  : Condition]
{
   --  True if (if_true, if_false) is a BDD that represents
   --  a single-condition decision that contains c

   --------------------
   -- Postconditions --
   --------------------

   dom [if_true] = c
   dom [if_false] = c
   c.if_false = Outcome_False
   c.if_true = Outcome_True
}

-------------------
-- build_bdd_not --
-------------------

pred build_bdd_not
[if_true,   if_false   : Condition -> (Condition + Decision_Outcome),
if_true_r,  if_false_r : Condition -> (Condition + Decision_Outcome)]
{
   --  True if (if_true, if_false) is a BDD for a decision "not D",
   --  where D's bdd is (if_true_r, if_false_r).

   -------------------
   -- Preconditions --
   -------------------

   B/is_totally_ordered_bdd [if_true_r, if_false_r]

   --------------------
   -- Postconditions --
   --------------------

   B/inner_edges [if_true]  = B/inner_edges [if_true_r]
   B/inner_edges [if_false] = B/inner_edges [if_false_r]

   if_true.Outcome_True  = if_true_r.Outcome_False
   if_true.Outcome_False = if_true_r.Outcome_True

   if_false.Outcome_True  = if_false_r.Outcome_False
   if_false.Outcome_False = if_false_r.Outcome_True
}

--------------------------------------
-- build_bdd_short_circuit_operator --
--------------------------------------

pred build_bdd_short_circuit_operator
[if_true,  if_false   : Condition -> (Condition + Decision_Outcome),
if_true_l, if_false_l : Condition -> (Condition + Decision_Outcome),
if_true_r, if_false_r : Condition -> (Condition + Decision_Outcome),
SC                    : Decision_Outcome]
{
   --  True if (if_true, if_false) is the BDD of "D ::= DL op BR",
   --  (if_true_l, if_false_l) being the BDD of DL
   --  (if_true_r, if_false_r) being the BDD of DR
   --  SC being Outcome_True  if op is "or else",
   --           Outcome_False if op is "and then"

   let NOT_SC = Decision_Outcome - SC {

      --------------------
      --  Preconditions --
      --------------------

      B/is_totally_ordered_bdd [if_true_l, if_false_l]
      B/is_totally_ordered_bdd [if_true_r, if_false_r]

      --  DL's BDD and DR's BDD are disjoint
      no nodes [if_true_l, if_false_l] & nodes [if_true_r, if_false_r]


      ---------------------
      --  Postconditions --
      ---------------------

      --  Build root node: the entry point is that of DL's BDD
      B/root [if_true, if_false] = B/root [if_true_l, if_false_l]

      --  Build exit edges: an exit edge labeled SC in DL's BDD is an
      --  exit edge labeled SC in D's BDD; the exit edges of BR's BDD
      --  are exit edges of D's BDD with the same labels.

      if_true.SC = if_true_l.SC + if_true_r.SC
      if_false.SC = if_false_l.SC + if_false_r.SC
      if_true.NOT_SC = if_true_r.NOT_SC
      if_false.NOT_SC = if_false_r.NOT_SC

      --  Build inner edges: Same as in BL's BDD and BR's BDD, plus
      --  the new edges connecting the two. These are build as follow:
      --  for each exit edges in DL's BDD that are *not* connected to SC maps,
      --  build an edge connected to DR's root in D's BDD.

      let root_r = root  [if_true_r, if_false_r] {
         let edges_lr_true =
            B/inner_edges [if_true_l] + B/inner_edges [if_true_r]
            {
               B/inner_edges [if_true] =
                  edges_lr_true + (if_true_l.NOT_SC -> root_r)
            }

         let edges_lr_false =
            B/inner_edges [if_false_l] + B/inner_edges [if_false_r]
            {
               B/inner_edges [if_false] =
                  edges_lr_false + (if_false_l.NOT_SC -> root_r)
            }
      }
   }
}


------------------------
-- build_bdd_and_then --
------------------------

pred build_bdd_and_then
[if_true,  if_false   : Condition -> (Condition + Decision_Outcome),
if_true_l, if_false_l : Condition -> (Condition + Decision_Outcome),
if_true_r, if_false_r : Condition -> (Condition + Decision_Outcome)]
{
   --  True if (if_true, if_false) is the bdd of a decision of the form
   --  "L and then R", (if_true_l, if_false_l) being the BDD of L,
   --  (if_true_r, if_false_r) being the BDD of R.

   build_bdd_short_circuit_operator [if_true, if_false,
                                     if_true_l, if_false_l,
                                     if_true_r, if_false_r,
                                     Outcome_False]
}

-----------------------
-- build_bdd_or_else --
-----------------------

pred build_bdd_or_else
[if_true,  if_false   : Condition -> (Condition + Decision_Outcome),
if_true_l, if_false_l : Condition -> (Condition + Decision_Outcome),
if_true_r, if_false_r : Condition -> (Condition + Decision_Outcome)]
{
   --  True if (if_true, if_false) is the bdd of a decision of the form
   --  "L or else R", (if_true_l, if_false_l) being the BDD of L,
   --  (if_true_r, if_false_r) being the BDD of R.

   build_bdd_short_circuit_operator [if_true, if_false,
                                     if_true_l, if_false_l,
                                     if_true_r, if_false_r,
                                     Outcome_True]
}

-------------------------------------------------------------------------------
--  private part

--------------------------
-- assertions and tests --
--------------------------

assert build_bdd_condition_check {
   --  Check that the graphs returned by build_bdd_condition are indeed BDDs

   all if_true, if_false : Condition -> (Condition + Decision_Outcome) |
      all c : Condition {
         build_bdd_condition [if_true, if_false, c]
         implies B/is_totally_ordered_bdd [if_true, if_false]
      }
}

check build_bdd_condition_check for 7

assert build_bdd_not_check {
   --  Check that the graphs returned by build_bdd_not are indeed BDDs

   all if_true, if_false,
       if_true_r, if_false_r : Condition -> (Condition + Decision_Outcome) {
          build_bdd_not [if_true, if_false, if_true_r, if_false_r]
          implies B/is_totally_ordered_bdd [if_true, if_false]
       }
}

check build_bdd_not_check for 7

assert build_bdd_short_circuit_operator_check {
   --  Check that the graphs returned build_bdd_and_then/build_bdd_or_else
   --  are indeed BDDs.

   all SC : Decision_Outcome |
      all if_true,  if_false,
          if_true_l, if_false_l,
          if_true_r, if_false_r : Condition -> (Condition + Decision_Outcome) {
             build_bdd_short_circuit_operator [if_true, if_false,
                                               if_true_l, if_false_l,
                                               if_true_r, if_false_r,
                                               SC]
             implies B/is_totally_ordered_bdd [if_true, if_false]
          }
}

check build_bdd_short_circuit_operator_check for 7

run build_bdd_condition for 5 but 9 Decision_Element
run build_bdd_not for 5 but 9 Decision_Element

private pred show_simple_and_then
[if_true,  if_false   : Condition -> (Condition + Decision_Outcome),
if_true_l, if_false_l : Condition -> (Condition + Decision_Outcome),
if_true_r, if_false_r : Condition -> (Condition + Decision_Outcome)]
{
   --  Show the BDD of "CL and then CR" when CL and CR are two
   --  conditions.

   some disj cl, cr : Condition {
      build_bdd_condition [if_true_l, if_false_l, cl]
      build_bdd_condition [if_true_r, if_false_r, cr]
   }

   build_bdd_and_then [if_true,   if_false,
                       if_true_l, if_false_l,
                       if_true_r, if_false_r]
}

run show_simple_and_then for 5 but 9 Decision_Element

private pred show_simple_or_else
[if_true,  if_false   : Condition -> (Condition + Decision_Outcome),
if_true_l, if_false_l : Condition -> (Condition + Decision_Outcome),
if_true_r, if_false_r : Condition -> (Condition + Decision_Outcome)]
{
   --  Show the BDD of "CL or else CR" when CL and CR are two
   --  conditions.

   some disj cl, cr : Condition {
      build_bdd_condition [if_true_l, if_false_l, cl]
      build_bdd_condition [if_true_r, if_false_r, cr]
   }

   build_bdd_or_else [if_true,   if_false,
                      if_true_l, if_false_l,
                      if_true_r, if_false_r]
}

run show_simple_or_else for 5 but 9 Decision_Element

private pred show_complex_and_then
[if_true,  if_false   : Condition -> (Condition + Decision_Outcome),
if_true_l, if_false_l : Condition -> (Condition + Decision_Outcome),
if_true_r, if_false_r : Condition -> (Condition + Decision_Outcome)]
{
   --  Show the BDD of "DL and then DR" when DL and DR are not two
   --  simple-condition decisions.

   B/is_bdd [if_true_l, if_false_l]
   B/is_bdd [if_true_r, if_false_r]

   #nodes[if_true_l, if_false_l] > 2
   #nodes[if_true_r, if_false_r] > 2

   build_bdd_and_then [if_true,   if_false,
                       if_true_l, if_false_l,
                       if_true_r, if_false_r]
}

run show_complex_and_then for 5 but 9 Decision_Element

run build_bdd_and_then for 5 but 9 Decision_Element
run build_bdd_or_else for 5 but 9 Decision_Element

