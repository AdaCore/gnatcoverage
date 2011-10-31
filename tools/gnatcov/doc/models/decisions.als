module xcov/models/decisions

--  This module provides a model of decision as a tree of conditions and
--  short-circuit operators.

private open util/graph[Decision_Element] as DE_GRAPH
private open util/sequniv as SEQ
private open util/integer as UI

private open xcov/models/decision_types

sig Decision {
   --  Decision, represented as a tree of Decision_Element's
   --  In this tree, the decision:
   --
   --        (A and then B) or else C
   --
   --   ...can be represented by the following graph:
   --
   --                Or_Else
   --                 /    \
   --                /      \
   --             And_Then   C
   --              /   \
   --             /     \
   --            A       B
   --
   --  Note that a distinction is obviously made between left and right
   --  edges from binary operators, as short-circuit operators handle
   --  their operands asymetrically.

   conditions : set Condition,
   bin_ops    : set Binary_Operator,
   un_ops     : set Unary_Operator,
   --  Sets of this decision's elements

   child_bin_left  : bin_ops lone -> lone (conditions + bin_ops + un_ops),
   child_bin_right : bin_ops lone -> lone (conditions + bin_ops + un_ops),
   child_un        : un_ops  lone -> lone (conditions + bin_ops + un_ops)
   --  Relates one nodes to its subnodes in the decision tree
}
{
   --  Non-empty decision
   some conditions

   --  Structural predicates: the "flat" graph of the decision is a tree
   DE_GRAPH/tree [graph [this]]

   --  Exactly two distinct children per binary operator
   all op : binary_ops [this] |
      one op.child_bin_right and one op.child_bin_left
   no child_bin_left & child_bin_right

   --  Exactly one child node per unary operator
   all op : unary_ops [this] | one op.child_un

   --  All nodes are reachable in tree
   no graph or conditions = Condition <: ran [graph [this]]
   bin_ops = Binary_Operator <: (ran [graph [this]] + dom [graph [this]])
   un_ops = Unary_Operator <: (ran [graph [this]] + dom [graph [this]])

   --  We may want to forbid sequences of not. They are not illegal; they
   --  are just useless...
   --  no Not.child_un & Not
}

--------------
-- elements --
--------------

fun elements [d : Decision] : set Decision_Element {
   --  Returns all decision elements in d

   conds [d] + unary_ops [d] + binary_ops [d]
}

-----------
-- graph --
-----------

fun graph [d : Decision] : Decision_Element -> Decision_Element {
   --  Return a "flat" graph representing all connections between decision
   --  elements without distinction of their nature. In this graph,
   --  the information about the place of operands (left or right)
   --  is lost.

   d.child_un + d.child_bin_left + d.child_bin_right
}

----------
-- root --
----------

fun root [d : Decision] : Decision_Element {
   --  Return d's root node

   elements [d] - elements [d].^(graph [d])
}

----------------
-- conditions --
----------------

fun conds [d : Decision] : set Condition {
   --  Return the set of conditions in d

   d.conditions
}

---------------
-- unary_ops --
---------------

fun unary_ops [d : Decision] : set Unary_Operator {
   --  Return the set of unary operators in d

   d.un_ops
}

-------------
-- not_ops --
-------------

fun not_ops [d : Decision] : set Not {
   --  Return the set of "not" operators in d

   unary_ops [d] & Not
}

----------------
-- binary_ops --
----------------

fun binary_ops [d : Decision] : set Binary_Operator {
   --  Return the set of binary operators in d

   d.bin_ops
}

------------------
-- and_then_ops --
------------------

fun and_then_ops [d : Decision] : set And_Then {
   --  Return the set of "and then" operators in d

   binary_ops [d] & And_Then
}

-----------------
-- or_else_ops --
-----------------

fun or_else_ops [d : Decision] : set Or_Else {
   --  Return the set of "or else" operators in d

   binary_ops [d] & Or_Else
}

--------------------
-- index_operands --
--------------------

fun index_operands
[d       : Decision,
c        : Decision_Element,
ordering : Int -> Decision_Element] : Int {
   --  if ordering records d's prefix polish form, returns the index
   --  of c in d.

   (ordering).(c.(graph [d]))
}

-----------------
-- is_ordering --
-----------------

pred is_ordering [d : Decision, ordering : Int -> Decision_Element] {
   --  True if ordering is a sequence that records d's prefix polish form

   --  ordering is sequence with no duplicates
   SEQ/isSeq [ordering]
   not hasDups [ordering]

   --  ordering orders d's elements, starting from root at index 0
   SEQ/elems [ordering] = elements [d]
   ordering.(DE_GRAPH/roots [graph [d]]) = 0

   --  Full ordering from d's structure:
   --  from any node, all nodes in the sub-decision from this node
   --  are ordered from the index of root node to the total number
   --  of elements in the sub-decision.
   all op : SEQ/elems [ordering] |
      all sub_op : sub_elements [d, op] |
         ordering.sub_op > ordering.op
         and ordering.sub_op <= UI/add [ordering.op, #sub_elements[d, op]]
}

--------------
-- sub_tree --
--------------

fun sub_elements
[d : Decision,
 c : Decision_Element]
: set Decision_Element {
   --  Return the elements of d's tree that are reachable from c

   c.^(graph [d])
}

-------------------------------------------------------------------------------
--  private part

-----------
-- facts --
-----------

private pred equals [d1, d2 : Decision] {
   --  True if d1 and d2 have the same elements

   d1.conditions = d2.conditions
   d1.child_bin_left = d2.child_bin_left
   d1.child_bin_right = d2.child_bin_right
   d1.child_un = d2.child_un
}

fact canonicalize {
   --  Two decisions with the same elements are equal

   all d1, d2 : Decision | equals [d1, d2] => d1 = d2
}

--------------------------
-- assertions and tests --
--------------------------

private pred show_simple_condition_decision [d : Decision] {
   --  Show a decision that contains only one decision, no operator

   #conditions [d] = 1
   #not_ops [d] = 0
}

run show_simple_condition_decision for 5 but 9 Decision_Element

private pred show_decision [d : Decision] {
   --  Show a decision
   #graph [d] > 3
}

run show_decision for 5 but 9 Decision_Element

private pred show_decisions_with_ordering
[d1, d2, d3 : Decision,
o1, o2, o3  : Int -> Decision_Element] {
   --  Show 3 decisions and their prefix polish form

   show_decision [d1]
   show_decision [d2]
   show_decision [d3]
   is_ordering [d1, o1]
   is_ordering [d2, o2]
   is_ordering [d3, o3]
}

run show_decisions_with_ordering for 9

assert Binary_Op_Dom_Symetry {
   --  Sanity check
   --  Check that the relations that represent left children and right
   --  children in a decision have the same domain

   all d : Decision | dom [d.child_bin_left] = dom [d.child_bin_right]
}

check Binary_Op_Dom_Symetry for 9

