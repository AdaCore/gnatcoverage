**Exercise MCDC over a nest of IF, WHILE and CASE statements**

Exercise a simple IF statement nested in a CASE alternative, conditioned by an
outer IF executed as part of a WHILE loop. We implement a double testing
strategy, with:

* Statement coverage-oriented scenarios, exercising SC possibilities (loop
  entered or not, IF part of an IF statement reached or not, CASE statement
  alternatives reached or not, ...) and combinations of these.

* Decision coverage oriented scenarios, exercising DC and MCDC possibilities
  on a given decision (True only, False only, both True and False with various
  degrees of condition independence) and combinations of these.
