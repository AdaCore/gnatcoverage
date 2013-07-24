Exercise a toplevel If statement, whose Else sequence contains a For Loop, whose body contains an IF statement controlling access to a Case statement
======================================================================================================================================================

Exercise a top-level If statement with an else part containing a For loop,
whose body includes an inner If statement that controls access to a Case
statement.

We implement a double testing strategy, with:

# Statement coverage-oriented scenarios, exercising SC possibilities (loop
  entered or not, If part of an If statement reached or not, Case statement
  alternatives reached or not, ...)  and combinations of these.

# Decision coverage-oriented scenarios, exercising DC possibilities
  (a given decision True only, False only, or both True and False) and
  combinations of these.

