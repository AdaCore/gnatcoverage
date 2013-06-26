Exercise a toplevel If statement with an else sequence embedding a For loop,
============================================================================

Exercise a toplevel If statement with an else sequence embedding a For loop,
whose body features an inner If statement that controls access to a Case
statement eventually.

We implement a double testing strategy, with:

# Statement coverage oriented scenarii, exercising SC possibilities (loop
  entered or not, If part of an If statement reached or not, Case statement
  selections reached or not, ...)  and combinations of such.

# Decision coverage oriented scenarii, exercising DC possibilities (such
  or such decision True only, False only, or both True and False) and
  combinations of such.

