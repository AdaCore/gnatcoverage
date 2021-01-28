module xcov/models/decision_types

--  This module provides declaration of atoms that are used to model
--  decisions with short-circuit operators.

abstract sig Decision_Outcome {}
one sig Outcome_True extends Decision_Outcome {}
one sig Outcome_False extends Decision_Outcome {}

abstract sig Decision_Element {}

abstract sig Binary_Operator extends Decision_Element {}
abstract sig Unary_Operator extends Decision_Element {}

sig Or_Else extends Binary_Operator {}
sig And_Then extends Binary_Operator {}

sig Not extends Unary_Operator {}

sig Condition extends Decision_Element {}

