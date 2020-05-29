Check the selection of Units Of Interest by command line options
================================================================

Verify that the tool produces expected reports for units of interest
selected with a variety of command line switches combinations,

Setup to exercise a system with two families of units implementing
simple operations on abstractions of Integer or Boolean values, each
stored in a separate directory with a dedicated regular (not library)
project file to expose the subsystem.

Both abstractions actually use a third one to maintain a counter of
requested operations. We have an overall directory/project structure
as follows::

  counters/counters.gpr  # Basic "Counter" asbtraction

  intops/intops.gpr      # Operations on Integer values, using Counters

  boolops/boolops.gpr    # Operations on Boolean values, using Counters


Main subprograms driving tests are controlled by a dedicated
project file, so we have the following project file structure
for each testcase::

            (withs)
  main.gpr ----+----> intops.gpr ----> counters.gpr
               |
               +----> boolops.gpr ---> counters.gpr


The variety of checks is achieved with various combinations
of :option: `-P`, :option:`--projects`, :option:`--no-subprojects` and
:option:`--units`.


