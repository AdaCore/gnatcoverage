SC requirements for ARM chap. 12 : Generic Units
================================================


For a given `generic` source (package or subprogram), the tool
aggregates the coverage achieved by different instantiations of the
entities offered by this source.

Statements or declarations within generic source code shall only be
reported uncovered when they are never executed at all, regardless of
the instantiations from which the execution might originate.


.. rubric:: Testing Strategy

We check a variety of statement constructs in different kinds of generic
sources (packages and subprograms) through library-level and local
instantiations.

We verify that a statement in a generic source is reported uncovered
only if it is exercised through none of the instances.

.. qmlink:: TCIndexImporter

   *
