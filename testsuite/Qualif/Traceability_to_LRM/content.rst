.. _lrm-traceability:

Traceability to Language Reference Manual 
=========================================

The table below demonstrates the TOR's coverage of a specific version of the
Ada language, keyed to the Table of Contents of the corresponding Language
Reference Manual (LRM).

Each row in the table identifies an LRM section, indicates whether the
features defined in that section are applicable to Structural Coverage
Analysis (SCA) (possible answers are "yes", "no", or "partial"), provides a
comment explaining the rationale for "no" or "partial", and which TORs and
test cases exercise the features for "yes" or "partial".

A feature is considered to be applicable to SCA if it is permitted by the
runtime library profile and if it introduces items of relevance to statement
or decision coverage, according to the following criteria:

* A feature is applicable to statement coverage if it is either a statement, a
  declaration that results in initialization code, or a pragma or
  representation_clause that contains an expression that needs to be evaluated
  at run time.

* A feature is applicable to decision coverage if it corresponds to a decision
  or condition, i.e., it is a language construct delivering a result of a
  Boolean type. The relevant features are thus logical operators, short
  circuit control forms, relational operators, membership tests,
  boolean-valued attributes, boolean-valued array elements and selected
  components, functions delivering a boolean result, and simple Boolean
  variables.

Because of the structure of the LRM, the description of the semantics of
some features is spread over several sections. To avoid redundancy, only
one of the sections is identified as applicable; the other sections are
classified as non-applicable with a comment indicating the associated
primary section for which SCA requirements are covered.

A large part of the Ada LRM is devoted to constructs with static
semantics but no run-time effect; since there are no SCA requirements
for these features, they are regarded as not applicable.

In many cases a feature that results in generated code is not applicable
to SCA because it can only occur in contexts where SCA obligations will
be associated with the surrounding construct. For example an array
aggregate (section 4.3.3) can only occur as a constituent of a statement
or a decision, it is not itself a statement or decision. Thus any SCA
requirements for the aggregate will be associated with the enclosing
statement or decision.

An LRM section is sometimes exercised directly by (i.e., directly traceable
to) its associated TORs, and sometimes indirectly by TORs that are also
testing other features. Indirect TORs arise for the sections in Chapter 4
(Names and Expressions). Further, because many features are naturally used
in a variety of tests, there is frequently more than one TOR that
exercises a given LRM section.

.. qmlink:: LRMTableImporter

   :allclass:LRM_Section

