** Exercise a toplevel IF with an ELSE sequence embedding a FOR loop, whose body features an inner IF statement that controls access to a CASE**

We implement a double testing strategy, with:

* Statement coverage-oriented scenarios, exercising SC possibilities (loop
  entered or not, If part of an If statement reached or not, Case statement
  alternatives reached or not, ...) and combinations of these.

* Decision coverage-oriented scenarios, exercising DC possibilities
  (a given decision True only, False only, or both True and False) and
  combinations of these.

