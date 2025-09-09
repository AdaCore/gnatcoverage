Requirements for the :option:`--subdirs` switch
=====================================================

The tool shall handle the :option:`--subdirs` command line switch,
following similar semantics as for gprbuild: with :option:`--subdir=<sd>`,
everything which should normally be performed within a project's
object directory "obj" is performed within "obj/<sd>" instead.

.. rubric:: Testing Strategy

We validate the tool's behavior with a small set of testcases
where --subdirs is passed to all commands, starting from gprbuild
which alters the location of the executable to run amongst other
things.

.. qmlink:: TCIndexImporter

   *
