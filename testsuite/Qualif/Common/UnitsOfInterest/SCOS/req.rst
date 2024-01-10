Use of SCO files to state Units Of Interest
===========================================

As an alternative to the use of project file facilities to designate
Units Of Interest for an analysis, the tool shall let users specify
those units by listing their corresponding .ali files with a set of
:option:`--scos=@<ali-filename>` and
:option:`--scos=@<response-filename>` command line switches which
accumulate.


.. rubric:: Testing Strategy

We exercise a simple program which comprises a basic functional unit
and a test driver achieving partial mcdc over the functional code.

Each testcase exposes the program in a different project structure,
influencing the location of .ali files for the unit, then checks a
variety of ways to convey the SCOs, providing ali files directly only,
a response file only, or a mix of direct ali files and response file.

.. qmlink:: TCIndexImporter

   *
