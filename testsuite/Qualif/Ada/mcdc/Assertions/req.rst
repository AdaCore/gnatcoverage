MCDC requirements regarding Assertions
======================================

On top of the DC definitions and requirements regarding assertions, there
shall be no MCDC coverage obligation attached to assertion aspects or pragmas for
which the controlling expression is never evaluated.

.. rubric:: Testing Strategy

The same general control mechanisms as for DC apply with MCDC, so we just
verify here the absence of decision or condition violation on disabled
assertions controlled by a multi-conditions expression while checking that
regular MCDC assessments still operate as expected.

.. qmlink:: TCIndexImporter

   *

