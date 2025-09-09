Use of GPR general project files facilities
===========================================

Project files may be used with GNATcoverage for other purposes
than the specification of Units Of Interest, in particular:

* Attributes in the Coverage package of the *root project file*
  (designated by a -P option), may be used to specify default switches
  that would otherwise be provided by on the command line switches
  (for example, :option:`--level` or :option:`--annotate`);

* A number of general project file facilities which make sense for
  gnatcov commands as well are honored as such (for example, the
  :option:`--subdirs` switch or
  :option:`Main`/:option:`Target`/:option:`RTS` attributes).

This incurs a set of related requirements summarized in the following table:

.. qmlink:: SubsetIndexImporter

   GnatcovSwitches
   MainAttr
   ConfigAttr
   Subdirs

