module xcov/models/bdd_decs
--  This modules provide a model that associates decisions to their
--  corresponding BDDs.

open xcov/models/bdd_relations[Condition, Decision_Outcome]
open xcov/models/bdds
open xcov/models/decisions
open xcov/models/build_bdd
open util/ternary as ter

private open xcov/models/decision_types

sig Dec_BDD {
   --  Associate a decision to its corresponding BDD, using the construction
   --  provided by build_bdd.

   decision : Decision,
   --  Considered decision

   bdd : BDD,
   --  BDD built from the considered decision

   construction_branch_true : Decision_Element ->
      (Condition -> (Condition + Decision_Outcome)),
   construction_branch_false : Decision_Element ->
      (Condition -> (Condition + Decision_Outcome))
   --  Relation that associates, to each element of the considered decision,
   --  the BDD that corresponds to the sub-decision that starts from this
   --  element (i.e. the sub-decision whose root node is this decision
   --  element). This models the intermediate step used to built the
   --  BDD that represents the considered decision.
   --
   --  This relation allows to express relations between intermediate "steps"
   --  of the BDD build from a recursive build procedure without having
   --  support for recursion in predicates (which is the case in Allow:
   --  the analyzer cannot support predicates that recursively calls
   --  themselves).
}
{
   --  Only elements of the considered decision are related to an
   --  intermediate step of the bdd build.
   ter/dom [construction_branch_true] = elements [decision]
   ter/dom [construction_branch_false] = elements [decision]

   --  The final step of the bdd build is the construction associated
   --  to the root node.
   bdd.if_true = root [decision].construction_branch_true
   bdd.if_false = root [decision].construction_branch_false

   --  From each element in the considered dcision, the intermediate
   --  step of the bdd build verifies the "build_bdd" predicates for its
   --  kind.
   all c : conditions [decision] |
      build_bdd_condition [c.construction_branch_true,
                           c.construction_branch_false,
                           c]

   all op : not_ops [decision] |
      let r = op.(decision.child_un) |
         build_bdd_not [op.construction_branch_true,
                        op.construction_branch_false,
                        r.construction_branch_true,
                        r.construction_branch_false]

   all op : and_then_ops [decision] |
      let l = op.(decision.child_bin_left) |
         let r = op.(decision.child_bin_right) |
            build_bdd_and_then [op.construction_branch_true,
                                op.construction_branch_false,
                                l.construction_branch_true,
                                l.construction_branch_false,
                                r.construction_branch_true,
                                r.construction_branch_false]

   all op : or_else_ops [decision] |
      let l = op.(decision.child_bin_left) |
         let r = op.(decision.child_bin_right) |
            build_bdd_or_else [op.construction_branch_true,
                               op.construction_branch_false,
                               l.construction_branch_true,
                               l.construction_branch_false,
                               r.construction_branch_true,
                               r.construction_branch_false]
}

-------------------------------------------------------------------------------
--  private part

--------------------------
-- assertions and tests --
--------------------------

private pred show_nots_bdd [db : Dec_BDD] {
   --  Show the BDD of a decision that contains only a (potentially empty )set
   --  not ops and 1 condition

   #conditions [db.decision] = 1
}

run show_nots_bdd for 7 but 1 BDD, 1 Decision

private pred show_and_then_bdd [db : Dec_BDD] {
   --  Show the BDD of a decision of the form "CL and then CR", CL and CR
   --  being two conditions

   #conditions [db.decision] = 2
   #and_then_ops [db.decision] = 1
   #not_ops [db.decision] = 0
}

run show_and_then_bdd for 7 but 1 BDD, 1 Decision

private pred show_or_else_bdd [db : Dec_BDD] {
   --  Show the BDD of a decision of the form "CL or else CR", CL and CR
   --  being two conditions

   #conditions [db.decision] = 2
   #or_else_ops [db.decision] = 1
   #not_ops [db.decision] = 0
}

run show_or_else_bdd for 7 but 1 BDD, 1 Decision

private pred show_complex_dec_bdd [db : Dec_BDD] {
   --  Show a BDD that uses every possible short-circuit boolean operator

   --  Some info about the simulations of this predicate:
   --
   --  * For 9 Decision_Element"
   --  Solver=minisat(jni) Bitwidth=4 MaxSeq=7 SkolemDepth=1 Symmetry=20
   --  1864058 vars. 13017 primary vars. 4724305 clauses. 27625ms.
   --  Instance found. Predicate is consistent. 10172ms.
   --
   --  * For 12 Decision_Element"
   --  Solver=minisat(jni) Bitwidth=4 MaxSeq=7 SkolemDepth=1 Symmetry=20
   --  5125322 vars. 29118 primary vars. 13624567 clauses. 78281ms.
   --  Instance found. Predicate is consistent. 40094ms.
   --
   --  It's interesting to see that SAT solvers are pretty good at solving
   --  the millions of constraint generated for the whole contruction tree...

   #and_then_ops [db.decision] > 0
   #or_else_ops [db.decision] > 0
   #not_ops [db.decision] > 0
}

run show_complex_dec_bdd for 7 but 1 BDD, 1 Decision, 9 Decision_Element

private pred show_diamond_decision [db: Dec_BDD] {
   --  Show a decision whose BDD that has a diamond

   --  Run for for 12 Decision_Element:
   --
   --  Solver=minisat(jni) Bitwidth=4 MaxSeq=5 SkolemDepth=1 Symmetry=20
   --  3667234 vars. 21058 primary vars. 9742408 clauses. 55593ms.
   --  Instance found. Predicate is consistent. 10594ms.

   has_diamond [db.bdd]
}

run show_diamond_decision for 5 but 1 BDD, 1 Decision, 9 Decision_Element

--  The following predicates/assertions aims at characterizing diamonds
--  in BDDs from the structure of the decisions. Diamonds is BDD being the
--  case of non-equivalence between OBC and MC/DC, being able to characterize
--  this case at the decision level is quite useful.

private pred show_no_diamond_decision [db: Dec_BDD] {
   --  Show that there are decision that contains both "and then" and
   --  "or else", and no "not", whose BDD contains no diamond. This
   --  proves that a precise characterization cannot be "only one kind
   --  of operators in the normal negative form of the decision."
   --  (only one kind of operators does imply no diamond; but there is
   --  no equivalence).

   not has_diamond [db.bdd]
   no not_ops [db.decision]
   some and_then_ops [db.decision]
   some or_else_ops [db.decision]
}

run show_no_diamond_decision for 5 but 1 BDD, 1 Decision, 7 Decision_Element

private pred mixed_left_operators [d : Decision] {
   --  True if we can find the following pattern in the decision:
   --    (D1 and then D2) or else D3
   --    (D1 or else D2) and then D3

   (some op : and_then_ops [d] |
      op.(d.child_bin_left) in or_else_ops [d])
   or
   (some op : or_else_ops [d] |
      op.(d.child_bin_left) in and_then_ops [d])
}

private pred diamonds_but_no_mixed_left_operators [db : Dec_BDD] {
   --  Considering only decision with no not operators (which is equivalent
   --  to only considering decisions in NNF), show that there are cases where
   --  there are decisions with no pattern of the form:
   --
   --    (D1 and then D2) or else D3
   --    (D1 or else D2) and then D3
   --
   --  ...that are still showing diamonds in their BDDs.

   no not_ops [db.decision]
   has_diamond [db.bdd]
   not mixed_left_operators [db.decision]
}

run diamonds_but_no_mixed_left_operators for 5 but 1 BDD, 1 Decision,
                                         7 Decision_Element

private pred dual_operator_on_left [d : Decision] {
   --  True if there is one sub-decision of the form "DL and then DR"
   --  such that the "or else" operator appears in DL (*anywhere* in DL);
   --  or if there is one sub-decision of the form "DL or else DR"
   --  such that the "and then" operator appears in DL.

   (some op : and_then_ops [d] |
      let left = op.(d.child_bin_left) |
         some (left.*(graph[d]) & or_else_ops [d]))
   or
   (some op : or_else_ops [d] |
      let left = op.(d.child_bin_left) |
         some (left.*(graph[d]) & and_then_ops [d]))
}

run dual_operator_on_left for 5 but 1 BDD, 1 Decision, 7 Decision_Element

assert diamonds_implies_dual_operator_on_left {
   --  Considering only decision with no not operators (which is equivalent
   --  to only considering decisions in NNF), assert that having a diamond
   --  exactly means that there is a sub-decision "DL op DR" such that
   --  DL contains the opposite operator ("or else" for "and then", "and then"
   --  for "or else"). This assertion can indeed be proved to be True in the
   --  general case and determine the correct characterization of diamonds
   --  from the structure of the decision.

   all db : Dec_BDD {
      (has_diamond [db.bdd]
       and no not_ops [db.decision])
      <=> (dual_operator_on_left [db.decision]
           and no not_ops [db.decision])
   }
}

check diamonds_implies_dual_operator_on_left for 5 but 1 BDD, 1 Decision,
                                                       7 Decision_Element
