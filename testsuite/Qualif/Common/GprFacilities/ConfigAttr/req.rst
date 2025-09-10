Requirements for Target and Runtime attributes
====================================================

For cross configurations, unless an explicit :option:`--target` or
:option:`--RTS` option is provided on the command line, the tool shall
assume their respective values from a :option:`Target` or a
:option:`Runtime` attribute when it is available. This could be
provided either by the root project file designated by :option:`-P`,
or otherwise by a configuration file designated by :option:
`--config`.

An explicit option provided on the command line always takes precedence.

.. rubric:: Testing Strategy

We validate this requirement by checking the Target attribute in particular,
used specifically by gnatcov run commands. The Runtime attribute, needed for
proper interpretation of project hierarchies, is handled similarily.

.. qmlink:: TCIndexImporter

   *
