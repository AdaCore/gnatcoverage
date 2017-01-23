SC requirements for ARM chap. 12 : Generic Units
================================================


%REQ_ID%

Unless requested otherwise by a command line argument to `gnatcov coverage`,
the tool aggregates for a given generic source the coverage achieved by
different instantiations of the entities offered by this source.

In this default mode, Statements or declarations within generic source code
shall only be reported uncovered when they are never executed at all,
regardless of the instantiations from which the execution might originate.

Conversely, with `-S instance` on the `gnatcov coverage` command line, the
tool shall perform a separate assessment and report about coverage violations
for each instantiation independently from the others.


.. rubric:: Testing Strategy

We check a variety of statement constructs in different kinds of generic
sources (packages and subprograms) through library-level and local
instantiations, some in default, aggregation, mode only, some both in
aggregation and per-instance modes.

In aggregation mode, we verify that a statement in a generic source is
reported uncovered only if it is exercised through none of the instances.

In per-instance mode, we exercise execution of a generic statement through
multiple combinations of instances and verify that the tool reports the
expected set of coverage violations for each distinct instance. Variations
within one instance is achieved by calling a particular set of subprograms
exposed by the instance, or by the use of conditional constructs within
subprograms.

.. qmlink:: TCIndexImporter

   *
