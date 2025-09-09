Requirements for Main attributes in project files
=======================================================

The gnatcov run command shall handle a Main attribute in the root
project file.

In absence of an explicit executable name on the command line, one is
determined from the Main attribute if there is one and if it lists a
single main unit. An error is raised otherwise.

An explicit executable name provided on the command line is always
honored, regardless of the root project file's contents in terms of
Main attribute (without any Main attribute or with a Main attribute
listing either a single or multiple main units).

.. rubric:: Testing Strategy

We verify the behavior of the tool thanks to the following set of
testcases, where we mix situations with project files containing

- no Main attribute,

- a Main attribute listing a single main unit, or

- a Main attribute listing multiple main units,

and checking for each case how the tool behaves when provided
with an explicit executable name on the command line or not.

.. qmlink:: TCIndexImporter

   *

