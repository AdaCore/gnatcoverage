DC expectations for mixes of statement and decision constructs representative
=============================================================================

DC expectations for mixes of statement and decision constructs representative
of real application code.

%(req-headline)s

The Core requirements are honored on programs mixing arbitrary Ada constructs
together, with arbitrary levels of syntactic nesting (such as loops within
tests within subprograms etc).

%(tstrategy-headline)s

We exercise multiple cases of functional code featuring a variety of
constructs nested within each other (For, While, Case, If), and for every case
check that the behavior matches expectations in a wide range of possible
situations:

* Loops entered or not,

* Possible case selections taken alone or combined with others,

* If controls evaluated True only, False only or both,

* Combinations of these allowed by the nesting structure.

%(tc-index)s

