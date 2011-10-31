module xcov/models/bdd_dec_reachability

private open xcov/models/bdd_relations[Condition, Decision_Outcome]
private open xcov/models/decision_types
private open xcov/models/build_bdd
private open util/ternary as ter


--  This module models reachibility properties in a BDD built from
--  a decision with short-circuit operators. It shows the following property:
--
--  Independent_Outcome_Reachability
--    For independent conditions, there is a path from each BDD node to
--    an exit edge labeled TRUE and to an exit edge labeled FALSE,
--    such that the two paths start with distinct edges from the node.

-------------------------------
-- independent_outcome_reach --
-------------------------------

pred independent_outcome_reach
[if_true, if_false : Condition -> (Condition + Decision_Outcome),
node               : Condition,
to_true            : Condition -> (Condition + Decision_Outcome),
to_false           : Condition -> (Condition + Decision_Outcome)]
{
   --  True if to_true and to_false are two paths from node such that
   --  these two paths start with distinct edges from the node and
   --  reach two different outcomes: Outcome_True for to_true,
   --  Outcome_False for to_false.

   is_totally_ordered_bdd [if_true, if_false]
   is_path [if_true, if_false, node, to_true]
   is_path [if_true, if_false, node, to_false]

   Decision_Outcome <: ran [to_true] = Outcome_True
   Decision_Outcome <: ran [to_false] = Outcome_False
   node.to_true != node.to_false
}

---------------------------------------------
-- independent_outcome_reachability_vector --
---------------------------------------------

pred independent_outcome_reachability_vector
[if_true        : Condition -> (Condition + Decision_Outcome),
if_false        : Condition -> (Condition + Decision_Outcome),
to_true_vector  : Condition -> (Condition -> (Condition + Decision_Outcome)),
to_false_vector : Condition -> (Condition -> (Condition + Decision_Outcome))]
{
   --  Same as independent_outcome_reach, but for each node of the BDD
   --  represented by (if_true, if_false).
   --
   --  An outcome reachability vector is a relation that associates each node
   --  of a BDD to the two paths from this node to Outcome_True and
   --  Outcome_False.

   ter/dom [to_true_vector] = nodes [if_true, if_false]
   ter/dom [to_false_vector] = nodes [if_true, if_false]
   all c : nodes [if_true, if_false] |
      independent_outcome_reach [if_true, if_false,
                                 c, c.to_true_vector, c.to_false_vector]
}

--  The property that we would like to check is that, for each totally
--  ordered BDD, there is a path from each BDD node to Outcome_True
--  and an other path to Outcome_False, such that the two paths start with
--  distinct edges from the node.
--
--  Expressed as an assertion, this would be something like:
assert check_independent_outcome_reachability
{
   all if_true, if_false : Condition -> (Condition + Decision_Outcome) |
      all node : Condition |
         some to_true, to_false : Condition -> (Condition + Decision_Outcome) |
            independent_outcome_reach [if_true, if_false,
                                       node, to_true, to_false]
}
--  ...this assertion, however, that cannot be checked by Alloy; it requires
--  an higher-order quantification that cannot be skolemized. We will
--  have to explicitely build these two paths. They will be built using
--  the structure of a decision and following the build procedure of a BDD:
--  * exhibit a path to both outcomes in the case of a simple condition
--    decision;
--  * assuming that we can exhibit such paths for each condition of a decision
--    D, show that we can build such paths for "not D";
--  * assuming that we can exhibit such paths for DL and DR, show that we can
--    build such paths for "DL and then DR" and "DL or else DR".
--
--  In other words (or seeing it differently), in the same process of
--  building a BDD from a decision, we build the paths for each of its
--  decisions to each of its outcomes.

------------------------------------------
-- build_outcome_reachability_condition --
------------------------------------------

pred build_outcome_reachability_condition
[to_true_vector   : Condition -> (Condition -> (Condition + Decision_Outcome)),
to_false_vector   : Condition -> (Condition -> (Condition + Decision_Outcome)),
c                 : Condition,
if_true, if_false : Condition -> (Condition + Decision_Outcome)]
{
   --  Considering c and the BDD (if_true, if_false) built for the
   --  corresponding simple condition decision, build to_true_vector
   --  and to_false_vector as this BDD's outcome reachability vector.

   -------------------
   -- preconditions --
   -------------------

   build_bdd_condition [if_true, if_false, c]

   --------------------
   -- postconditions --
   --------------------

   to_true_vector = c -> (c -> Outcome_True)
   to_false_vector = c -> (c -> Outcome_False)
}

------------------------------------
-- build_outcome_reachability_not --
------------------------------------

pred build_outcome_reachability_not
[to_true_vector   : Condition -> (Condition -> (Condition + Decision_Outcome)),
to_false_vector   : Condition -> (Condition -> (Condition + Decision_Outcome)),
if_true, if_false : Condition -> (Condition + Decision_Outcome),

if_t_op, if_f_op  : Condition -> (Condition + Decision_Outcome),
to_t_vect_op      : Condition -> (Condition -> (Condition + Decision_Outcome)),
to_f_vect_op      : Condition -> (Condition -> (Condition + Decision_Outcome))]
{
   --  Considering a decision D = not D_op, and the following BDDs:
   --  * (if_t_op, if_f_op): D_op's BDD;
   --  * (if_true, if_false): D's BDD, as built from D_op;
   --
   --  ...and assuming that (to_t_vect_op, to_f_vect_op) is the outcome
   --  reachability vectors for D_op's BDD...
   --
   --  ...build the outcome reachability vector for D's BDD.

   -------------------
   -- preconditions --
   -------------------

   build_bdd_not [if_true, if_false, if_t_op, if_f_op]
   independent_outcome_reachability_vector [if_t_op, if_f_op,
                                            to_t_vect_op, to_f_vect_op]

   --------------------
   -- postconditions --
   --------------------

   --  Set the result range:

   ter/dom [to_true_vector] = nodes [if_true, if_false]
   ter/dom [to_false_vector] = nodes [if_true, if_false]

   --  Fill the relation:

   all c : nodes [if_true, if_false] |
      let to_true_op = c.to_t_vect_op |
      let to_false_op = c.to_f_vect_op |
      let to_true = c.to_true_vector |
      let to_false = c.to_false_vector {
         --  To build "not D"'s BDD, we compute D's BDD and switch its outcome.
         --  So, to each path that goes to Outcome_True in D's BDD, we can
         --  associate a path that goes to Outcome_False in "not D"'s BDD:
         --  All we have to do it to switch this path's terminal node.
         --  Same thing for a pathe to Outcome_False in D's BDD.
         to_false = to_true_op ++ to_true_op.Outcome_True -> Outcome_False
         to_true = to_false_op ++ to_false_op.Outcome_False -> Outcome_True
      }
}

-------------------------------------------------------
-- build_outcome_reachability_short_circuit_operator --
-------------------------------------------------------

pred build_outcome_reachability_short_circuit_operator
[to_sc_vector     : Condition -> (Condition -> (Condition + Decision_Outcome)),
to_not_sc_vector  : Condition -> (Condition -> (Condition + Decision_Outcome)),
if_true, if_false : Condition -> (Condition + Decision_Outcome),

to_sc_vect_l      : Condition -> (Condition -> (Condition + Decision_Outcome)),
to_not_sc_vect_l  : Condition -> (Condition -> (Condition + Decision_Outcome)),
if_t_l, if_f_l    : Condition -> (Condition + Decision_Outcome),

to_sc_vect_r      : Condition -> (Condition -> (Condition + Decision_Outcome)),
to_not_sc_vect_r  : Condition -> (Condition -> (Condition + Decision_Outcome)),
if_t_r, if_f_r    : Condition -> (Condition + Decision_Outcome),

SC                : Decision_Outcome]
{
   --  Considering a decision D = DL op DR, and the following BDDs:
   --  * (if_t_l, if_f_l): DL's BDD;
   --  * (if_t_r, if_f_r): DL's BDD;
   --  * (if_true, if_false): D's BDD, as built from DL and DR;
   --
   --  ...and considering the following outcome reachability vectors:
   --  * (to_sc_vect_l, to_not_vect_l): outcome reachability vectors
   --                                   for DL's BDD;
   --  * (to_sc_vect_r, to_not_sc_vect_r): outcome reachability vectors
   --                                      for DR's BDD;
   --
   --  ...build the outcome reachability vector for D's BDD.

   -------------------
   -- preconditions --
   -------------------

   build_bdd_short_circuit_operator [if_true, if_false,
                                     if_t_l, if_f_l,
                                     if_t_r, if_f_r,
                                     SC]

   SC = Outcome_True implies {
      independent_outcome_reachability_vector [if_t_l, if_f_l,
                                               to_sc_vect_l, to_not_sc_vect_l]
      independent_outcome_reachability_vector [if_t_r, if_f_r,
                                               to_sc_vect_r, to_not_sc_vect_r]
   }

   SC = Outcome_False implies {
      independent_outcome_reachability_vector [if_t_l, if_f_l,
                                               to_not_sc_vect_l, to_sc_vect_l]
      independent_outcome_reachability_vector [if_t_r, if_f_r,
                                               to_not_sc_vect_r, to_sc_vect_r]
   }

   --------------------
   -- postconditions --
   --------------------

   --  Two cases:

   --  * either c in the right decision, in which case the path to
   --  Outcome_True and Outcome_False in "DL op DR"'s BDD is the same
   --  as the one in DR's BDD;
   all c : nodes [if_t_r, if_f_r] |
      let to_sc_right = c.to_sc_vect_r |
      let to_not_sc_right = c.to_not_sc_vect_r |
      let to_sc = c.to_sc_vector |
      let to_not_sc = c.to_not_sc_vector {
         to_sc = to_sc_right
         to_not_sc = to_not_sc_right
   }

   --  * or c in the left decision; in this case, the path to SC is
   --  is the same as in DL's, and the path to "not SC" can be built
   --  by concatenating the path to DR's root with the path from root
   --  to "not SC":

   all c : nodes [if_t_l, if_f_l] |
      let to_sc_left = c.to_sc_vect_l |
      let to_not_sc_left = c.to_not_sc_vect_l |
      let to_sc = c.to_sc_vector |
      let to_not_sc = c.to_not_sc_vector |
      let root_right_to_not_sc = root [if_t_r, if_f_r].to_not_sc_vect_r {
         to_sc = to_sc_left
         concat [to_not_sc, to_not_sc_left, root_right_to_not_sc]
      }

   --  ...and these are the only two cases. By construction, we know that
   --  nodes [if_true, if_false] =
   --     nodes [if_t_l, if_f_l] + nodes [if_t_r, if_f_r]
   ter/dom [to_sc_vector] =
      nodes [if_t_l, if_f_l] + nodes [if_t_r, if_f_r]
   ter/dom [to_not_sc_vector] =
      nodes [if_t_l, if_f_l] + nodes [if_t_r, if_f_r]
}

-----------------------------------------
-- build_outcome_reachability_and_then --
-----------------------------------------

pred build_outcome_reachability_and_then
[to_true_vector   : Condition -> (Condition -> (Condition + Decision_Outcome)),
to_false_vector   : Condition -> (Condition -> (Condition + Decision_Outcome)),
if_true, if_false : Condition -> (Condition + Decision_Outcome),

to_t_vect_l       : Condition -> (Condition -> (Condition + Decision_Outcome)),
to_f_vect_l       : Condition -> (Condition -> (Condition + Decision_Outcome)),
if_t_l, if_f_l    : Condition -> (Condition + Decision_Outcome),

to_t_vect_r       : Condition -> (Condition -> (Condition + Decision_Outcome)),
to_f_vect_r       : Condition -> (Condition -> (Condition + Decision_Outcome)),
if_t_r, if_f_r    : Condition -> (Condition + Decision_Outcome)]
{
   --  build_bdd_short_circuit_operator for SC = False

   -------------------
   -- preconditions --
   -------------------

   build_bdd_and_then [if_true, if_false,
                       if_t_l, if_f_l,
                       if_t_r, if_f_r]
   independent_outcome_reachability_vector [if_t_l, if_f_l,
                                            to_t_vect_l, to_f_vect_l]
   independent_outcome_reachability_vector [if_t_r, if_f_r,
                                            to_t_vect_r, to_f_vect_r]

   --------------------
   -- postconditions --
   --------------------

   build_outcome_reachability_short_circuit_operator
   [to_false_vector, to_true_vector,
   if_true, if_false,

   to_f_vect_l, to_t_vect_l,
   if_t_l, if_f_l,

   to_f_vect_r, to_t_vect_r,
   if_t_r, if_f_r,

   Outcome_False]
}

----------------------------------------
-- build_outcome_reachability_or_else --
----------------------------------------

pred build_outcome_reachability_or_else
[to_true_vector   : Condition -> (Condition -> (Condition + Decision_Outcome)),
to_false_vector   : Condition -> (Condition -> (Condition + Decision_Outcome)),
if_true, if_false : Condition -> (Condition + Decision_Outcome),

to_t_vect_l       : Condition -> (Condition -> (Condition + Decision_Outcome)),
to_f_vect_l       : Condition -> (Condition -> (Condition + Decision_Outcome)),
if_t_l, if_f_l    : Condition -> (Condition + Decision_Outcome),

to_t_vect_r       : Condition -> (Condition -> (Condition + Decision_Outcome)),
to_f_vect_r       : Condition -> (Condition -> (Condition + Decision_Outcome)),
if_t_r, if_f_r    : Condition -> (Condition + Decision_Outcome)]
{
   --  build_bdd_short_circuit_operator for SC = True

   -------------------
   -- preconditions --
   -------------------

   build_bdd_or_else [if_true, if_false,
                      if_t_l, if_f_l,
                      if_t_r, if_f_r]
   independent_outcome_reachability_vector [if_t_l, if_f_l,
                                            to_t_vect_l, to_f_vect_l]
   independent_outcome_reachability_vector [if_t_r, if_f_r,
                                            to_t_vect_r, to_f_vect_r]

   --------------------
   -- postconditions --
   --------------------

   build_outcome_reachability_short_circuit_operator
   [to_true_vector, to_false_vector,
   if_true, if_false,

   to_t_vect_l, to_f_vect_l,
   if_t_l, if_f_l,

   to_t_vect_r, to_f_vect_r,
   if_t_r, if_f_r,

   Outcome_True]
}


-------------------------------------------------------------------------------
--  private part

run independent_outcome_reach for 7

run build_outcome_reachability_condition for 7
run build_outcome_reachability_not for 7
run build_outcome_reachability_and_then for 7
run build_outcome_reachability_or_else for 7

--  These assertions checks, by structural induction, that the vector
--  built by build_outcome_reachability_* are indeed outcome reachability
--  vectors, i.e. that that the property Independent_Outcome_Reachability
--  is preserved. As this is checked for all possible build operations
--  of a BDD, this proves our property for all BDDs.

assert check_build_outcome_reachability_condition {
   --  Check that build_outcome_reachability_condition's result is
   --  indeed an outcome reachability vector

   all c : Condition |
      all if_true, if_false : Condition -> (Condition + Decision_Outcome) |
         all to_t_vect, to_f_vect :
            Condition -> (Condition -> (Condition + Decision_Outcome)) {
               build_outcome_reachability_condition [to_t_vect, to_f_vect,
                                                     c, if_true, if_false]
               implies
               independent_outcome_reachability_vector [if_true, if_false,
                                                       to_t_vect, to_f_vect]
      }
}

check check_build_outcome_reachability_condition for 7

assert check_build_outcome_reachability_not {
   --  Check that build_outcome_reachability_not's result is
   --  indeed an outcome reachability vector

   all if_true_op, if_false_op,
       if_true, if_false : Condition -> (Condition + Decision_Outcome) |
   all to_t_vect_op, to_f_vect_op,
       to_t_vect, to_f_vect :
          Condition -> (Condition -> (Condition + Decision_Outcome)) {
             build_outcome_reachability_not [to_t_vect, to_f_vect,
                                             if_true, if_false,

                                             if_true_op, if_false_op,
                                             to_t_vect_op, to_f_vect_op]
             implies
             independent_outcome_reachability_vector [if_true, if_false,
                                                      to_t_vect, to_f_vect]
       }
}

check check_build_outcome_reachability_not for 5

assert check_build_outcome_reachability_and_then {
   --  Check that build_outcome_reachability_and_then's result is
   --  indeed an outcome reachability vector

   all if_true_l, if_false_l,
       if_true_r, if_false_r,
       if_true, if_false : Condition -> (Condition + Decision_Outcome) |
   all to_t_vect_l, to_f_vect_l,
       to_t_vect_r, to_f_vect_r,
       to_t_vect, to_f_vect :
          Condition -> (Condition -> (Condition + Decision_Outcome)) {
             build_outcome_reachability_and_then [to_t_vect, to_f_vect,
                                                  if_true, if_false,

                                                  to_t_vect_l, to_f_vect_l,
                                                  if_true_l, if_false_l,

                                                  to_t_vect_r, to_f_vect_r,
                                                  if_true_r, if_false_r]
             implies
             independent_outcome_reachability_vector [if_true, if_false,
                                                      to_t_vect, to_f_vect]
       }
}

check check_build_outcome_reachability_and_then for 5

assert check_build_outcome_reachability_or_else {
   --  Check that build_outcome_reachability_or_else's result is
   --  indeed an outcome reachability vector

   all if_true_l, if_false_l,
       if_true_r, if_false_r,
       if_true, if_false : Condition -> (Condition + Decision_Outcome) |
   all to_t_vect_l, to_f_vect_l,
       to_t_vect_r, to_f_vect_r,
       to_t_vect, to_f_vect :
          Condition -> (Condition -> (Condition + Decision_Outcome)) {
             build_outcome_reachability_or_else [to_t_vect, to_f_vect,
                                                 if_true, if_false,

                                                 to_t_vect_l, to_f_vect_l,
                                                 if_true_l, if_false_l,

                                                 to_t_vect_r, to_f_vect_r,
                                                 if_true_r, if_false_r]
             implies
             independent_outcome_reachability_vector [if_true, if_false,
                                                      to_t_vect, to_f_vect]
       }
}

check check_build_outcome_reachability_or_else for 5
