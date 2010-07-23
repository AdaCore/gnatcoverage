module xcov/models/bdd_relations[BDD_Node, BDD_Term]

--  This module provides a model of binary decision diagrams as a pair of
--  relations (if_true, if_false) between nodes and {nodes + terminal nodes}
--  without reifying them as atoms.
--
--  Not represeting BDD as atoms avoids to have their complexity artificially
--  restrained by the number of their elements in the instance scope.

open util/relation

------------
-- is_bdd --
------------

pred is_bdd [if_true, if_false : BDD_Node -> (BDD_Node + BDD_Term)]
{
   --  True if (if_true, if_false) is a BDD

   let graph = if_true + if_false {
      --  Non empty
      some graph

      --  One root
      one node : dom [graph] | no graph.node

      --  Acyclicity
      no iden & ^graph

      --  No trivial node
      no if_false & if_true

      --  No pending node
      --  (Note that we cannot use fun nodes here, as this function
      --  takes advantage of the fact that this axiom is verified)
      all node : BDD_Node <: dom [graph] |
         one node.if_false and one node.if_true
      all node : BDD_Node <: ran [graph] |
         one node.if_false and one node.if_true
   }
}

----------------------------
-- is_totally_ordered_bdd --
----------------------------

pred is_totally_ordered_bdd
[if_true, if_false : BDD_Node -> (BDD_Node + BDD_Term)]
{
   --  True if (if_true, if_false) is a BDD  whose transitive closure
   --  is a total order over the set of this bdd's nodes.

   is_bdd [if_true, if_false]

   let edges = inner_edges [if_true + if_false] |
      totalOrder [*edges, nodes [if_true, if_false]]
}

-----------
-- nodes --
-----------

fun nodes
[if_true, if_false : BDD_Node -> (BDD_Node + BDD_Term)]
: set BDD_Node
{
   --  Return the nodes of (if_true, if_false)

   (if_true + if_false).((BDD_Node + BDD_Term))
}

---------------
-- sub_nodes --
---------------

fun sub_nodes
[if_true, if_false : BDD_Node -> (BDD_Node + BDD_Term),
n                  : BDD_Node]
: set BDD_Node
{
   --  Return the nodes of (if_true, if_false) that are reachable
   --  from n (n included).

   n.*(if_true + if_false)
}


----------
-- root --
----------

fun root
[if_true, if_false : BDD_Node -> (BDD_Node + BDD_Term)]
: set BDD_Node
{
   --  Return the root node of (if_true, if_false)

   nodes [if_true, if_false] - nodes [if_true, if_false].^(if_true + if_false)
}

---------------
-- next_node --
---------------

pred next_node
[if_true, if_false : BDD_Node -> (BDD_Node + BDD_Term),
n1, n2             : BDD_Node]
{
   --  True if n2 is one edge away from n1 in (if_true, if_false)

   let graph = if_true + if_false {
      n1 -> n2 in graph
   }
}

--------------------
-- reachable_from --
--------------------

pred reachable_from
[if_true, if_false : BDD_Node -> (BDD_Node + BDD_Term),
n1, n2             : BDD_Node]
{
   --  True if n2 can be reached from n1 in (if_true, if_false)

   let graph = if_true + if_false {
      n2 in n1.^graph
   }
}

--------------------------
-- more_than_one_father --
--------------------------

pred more_than_one_father
[if_true, if_false : BDD_Node -> (BDD_Node + BDD_Term),
node               : BDD_Node]
{
   --  True if node can be reached by more than one edge in (if_true, if_false)

   let graph = if_true + if_false {
      #(graph.node) > 1
   }
}

-----------------
-- has_diamond --
-----------------

pred has_diamond [if_true, if_false : BDD_Node -> (BDD_Node + BDD_Term)]
{
   --  True if one of (if_true, if_false) can be reached by two different
   --  paths.

   some n : nodes [if_true, if_false] |
      more_than_one_father [if_true, if_false, n]
}

-----------------
-- inner_edges --
-----------------

fun inner_edges
[edges  : BDD_Node -> (BDD_Node + BDD_Term)]
: BDD_Node -> (BDD_Node + BDD_Term)
{
   --  Return all edges that are not connected to a terminal outcome

   (BDD_Node -> BDD_Node) & edges
}

--------------------
-- terminal_edges --
--------------------

fun terminal_edges
[edges  : BDD_Node -> (BDD_Node + BDD_Term)]
: BDD_Node -> (BDD_Node + BDD_Term)
{
   --  Return all edges that are connected to a terminal outcome

   (BDD_Node -> BDD_Term) & edges
}

-----------------
-- is_sequence --
-----------------

pred is_sequence [next : BDD_Node -> (BDD_Node + BDD_Term)]
{
   --  True if next is the relation from one node to the next one
   --  in a sequence of nodes with no duplicates, ending with a BDD_Term.
   --  This structure is used to represent path into BDDs. Empty sequences
   --  are also accepted by this predicate.

   --  Acyclicity
   no iden & ^next

   --  Each node of the sequence has only one next node
   all node : dom [next] | one node.next

   --  At most one initial node
   lone dom [next] - ran [next]

   --  If the sequence is not empty, the terminal node is a BDD_Term
   no next or ((ran [next] - dom [next]) in BDD_Term)
}

-----------
-- elems --
-----------

fun elems [next : BDD_Node -> (BDD_Node + BDD_Term)] : set BDD_Node
{
   --  Return all nodes in a sequence

   next.(BDD_Node + BDD_Term)
}

------------
-- concat --
------------

pred concat [result, left, right : BDD_Node -> (BDD_Node + BDD_Term)]
{
   --  True if result represents the concatenation of two sequences
   --  (left and right).

   -------------------
   -- preconditions --
   -------------------

   is_sequence [left]
   is_sequence [right]

   --  Sequences are disjoint. Without this property, the
   --  concatenation of two sequences may not be a sequence
   --  (acyclicity may be broken).
   no dom [left] & dom [right]

   --------------------
   -- postconditions --
   --------------------

   no right implies result = left
   no left implies result = right
   some left and some right implies
   let last_left = left.BDD_Term |
      let first_right = elems [right] - elems [right].^right {
         result = left + right
            - (last_left -> last_left.left)
            + (last_left -> first_right)
      }
}

-------------
-- is_path --
-------------

pred is_path
[if_true, if_false : BDD_Node -> (BDD_Node + BDD_Term),
path               : BDD_Node -> (BDD_Node + BDD_Term)]
{
   --  True if path is a valid path through bdd (if_true, if_false)

   is_bdd [if_true, if_false]
   is_sequence [path]
   path in (if_true + if_false)
   root [if_true, if_false] in dom [path]
}

-------------------------------------------------------------------------------
--  private part

--------------------------
-- Assertions and tests --
--------------------------

fact two_terminal_nodes {
   #BDD_Term = 2
}

assert concat_builds_a_sequence {
   --  Check that the concatenation of two sequences returns a sequence

   all result, left, right : BDD_Node -> (BDD_Node + BDD_Term) |
      concat [result, left, right] implies is_sequence [result]
}

check concat_builds_a_sequence for 7

private pred show_bdd
[if_true, if_false : BDD_Node -> (BDD_Node + BDD_Term)]
{
   --  Show a BDD that contains at least three conditions

   is_bdd [if_true, if_false]
   #if_true > 2
}

run show_bdd for 5

pred show_path
[if_true, if_false : BDD_Node -> (BDD_Node + BDD_Term),
path               : BDD_Node -> (BDD_Node + BDD_Term)]
{
   --  Show a path through a BDD

   #if_true > 1
   #if_false > 1
   is_path [if_true, if_false, path]
}

run show_path for 5

private pred show_inner_edges
[if_true1, if_false1 : BDD_Node -> (BDD_Node + BDD_Term),
if_true2,  if_false2 : BDD_Node -> (BDD_Node + BDD_Term)]
{
   --  Show two BDDs that have the same inner edges

   is_bdd [if_true1, if_false1]
   is_bdd [if_true2, if_false2]
   nodes [if_true1, if_false1] = nodes [if_true2, if_false2]
   inner_edges [if_true1] = inner_edges [if_true2]
   inner_edges [if_false1] = inner_edges [if_false2]
}

run show_inner_edges for 5

assert one_terminal_node
{
   --  Check that the definition of a non-null sequence implies
   --  the unicity of its terminal node.

   all next : BDD_Node -> (BDD_Node + BDD_Term) |
      (some next and is_sequence [next]) implies {
         one BDD_Term & ran [next]
      }
}

check one_terminal_node for 5
