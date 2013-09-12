**********
Appendices
**********

.. _sample_sc_html_index:

Sample html index
=================

.. image:: sample_sc_html_index.png
   :scale: 80%

.. _sample_sc_html_unit:

Sample html annotated source
============================

.. image:: sample_sc_html_unit.png
   :scale: 80%


.. _target_specific_notes:

Target specific points of note
==============================

The following list summarizes points of note for each target where some
aspects of the build/execution/analysis process depart from the general
instructions.

For **powerpc-vxworks6**:

- Need to compile with -gno-strict-dwarf for source coverage analysis;

- Need to add a --kernel argument on |gcvrun|;

- The provided kernel has to be augmented with a specific module
  for gnatcov purposes. Please refer to the |gem| documentation for this
  part of the process;

- Support for the ``kernel`` Ada RTS and Downloadable Kernel Modules only.



