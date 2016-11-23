**Exercise MCDC over a nest of IF, FOR and CASE statements**

Exercise a toplevel IF with an ELSE sequence embedding a FOR loop, whose body
features an inner IF statement that controls access to a CASE statement.

We implement a double testing strategy, with:

* Statement coverage-oriented scenarios, exercising SC possibilities (loop
  entered or not, IF part of an IF statement reached or not, CASE statement
  alternatives reached or not, ...) and combinations of these.

* Decision coverage oriented scenarios, exercising DC and MCDC possibilities
  on a given decision (True only, False only, both True and False with various
  degrees of condition independence) and combinations of these.

