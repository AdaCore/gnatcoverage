module xcov/models/bdds

--  This module provides a model of binary decision diagrams reified as
--  atoms.

open xcov/models/bdd_relations[Condition, Decision_Outcome] as B
private open xcov/models/decision_types

sig BDD {
   --  Binary decision diagram

   nodes : set Condition,
   --  Set of nodes in BDD

   if_true    : nodes -> one (nodes + Decision_Outcome),
   --  Relation between each node and its outcome if true

   if_false   : nodes -> one (nodes + Decision_Outcome)
   --  Relation between each node and its outcome if false
}
{
   B/is_totally_ordered_bdd [if_true, if_false]
}

----------
-- root --
----------

fun root [bdd : BDD] : Condition {
   --  Return bdd's root node

   B/root [bdd.if_true, bdd.if_false]
}

------------
-- root_p --
------------

pred root_p [bdd : BDD, node : Condition] {
   --  True if node is the root node of bdd

   node = root [bdd]
}

---------------
-- next_node --
---------------

pred next_node [bdd : BDD, n1, n2 : (Condition + Decision_Outcome)] {
   --  True if n2 is one arrow away from n1 in bdd

   B/next_node [bdd.if_true, bdd.if_false, n1, n2]
}

--------------------
-- reachable_from --
--------------------

pred reachable_from [bdd : BDD, n1, n2 : (Condition + Decision_Outcome)] {
   --  True if n2 can be reached from n1 in bdd

   B/reachable_from [bdd.if_true, bdd.if_false, n1, n2]
}

--------------------------
-- more_than_one_father --
--------------------------

pred more_than_one_father [bdd : BDD, n : Condition] {
   --  True if n can be reached by more than one edge

   B/more_than_one_father [bdd.if_true, bdd.if_false, n]
}

-----------------
-- has_diamond --
-----------------

pred has_diamond [bdd : BDD] {
   --  True if one of bdd's node can be reached by two different paths

   B/has_diamond [bdd.if_true, bdd.if_false]
}

-------------------------------------------------------------------------------
--  private part

-----------
-- facts --
-----------

private pred equals [bdd_1, bdd_2 : BDD] {
   --  True if bdd_1 and bdd_2 have the same elements

   bdd_1.if_false = bdd_2.if_false
   bdd_1.if_true = bdd_2.if_true
   bdd_1.nodes = bdd_2.nodes
}

fact canonicalize {
  --  Two BDD with the same elements are equal

  all bdd_1, bdd_2 : BDD | equals [bdd_1, bdd_2] => bdd_1 = bdd_2
}

--------------------------
-- assertions and tests --
--------------------------

private pred show_bdd [] {
   --  A non-trivial set of BDDs

   #BDD.if_true > 0
   #BDD.if_false > 0
}

run show_bdd for 5 but 1 BDD

assert exactly_two_outcomes {
   --  Sanity check: both True and False are in a BDD. This is a consequence
   --  of two axioms : "No trivial node" and "Acyclicity"

   all bdd : BDD {
       Decision_Outcome in Condition.(bdd.(if_true + if_false))
   }
}

check exactly_two_outcomes for 7 but 1 BDD

assert one_penultimate_node {
   --  There is at least one node in bdd from which you can reach
   --  the two possible outcomes (Outcome_True, Outcome_False)

   all bdd : BDD | some n : bdd.nodes |
      (Outcome_True + Outcome_False) in n.(bdd.if_true + bdd.if_false)
}

check one_penultimate_node for 7 but 4 BDD

