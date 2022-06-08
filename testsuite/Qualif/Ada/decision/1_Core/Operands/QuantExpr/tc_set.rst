**Check DC on quantified expression used as decision operand**

Verify the proper handling of a quantified expression used as a
decision (e.g. controlling an IF statement), where the predicate
is thus a nested separate decision.

Check FORALL and FORSOME expressions iterating over array indexes (for
all/some I in Arr'Range) or over array element values (for all/some V
of Arr), controlled by a unary predicate.

For FORSOME expressions, exercise situations where:
- The outer expression evaluates both True and False,
- The outer expression evaluates False only, with the
  predicate always False,
- The outer expression evaluates True only, with the
  predicate always True,
- The outer expression evaluates True only, with the
  predicate True on the first item of a sequence,
- The outer expression evaluates True only, with the
  predicate True for an item in the middle of a sequence,
- The outer expression evaluates True only, with the
  predicate True on the last item of a sequence.

Similarily, for FORALL expressions, exercise situations where:
- The outer expression evaluates both True and False,
- The outer expression evaluates True only, with the
  predicate always True,
- The outer expression evaluates False only, with the
  predicate always False,
- The outer expression evaluates False only, with the
  predicate False on the first item of a sequence,
- The outer expression evaluates False only, with the
  predicate False for an item in the middle of a sequence,
- The outer expression evaluates True only, with the
  predicate False on the last item of a sequence.


.. qmlink:: TCIndexImporter

   *

