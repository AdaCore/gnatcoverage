*********************************************************
Source/Object Coverage common considerations & facilities
*********************************************************

.. _exemptions:

Exemption Regions
=================

In some circumstances, there are good and well understood reasons why proper
coverage of some source construct is not achievable, and it is convenient to
be able to abstract these coverage violations away from the genuine defects of
a testing campaign.  The |gcp| :dfn:`exemptions` facility was designed for
this purpose.

For Ada with the |gnat| compilers, coverage exemptions are requested for
sections of source by the insertion of dedicated pragmas:

- ``pragma Annotate (Xcov, Exempt_On, "justification text");`` starts a
  section, providing some justification text that will be recalled in coverage
  reports.

- ``pragma Annotate (Xcov, Exempt_Off);`` closes the current exemption section.

There may be no overlap between exemption regions.

Exempted regions are reported as blocks in both the annotated source and the
synthetic text reports, for both source and object coverage metrics.

In annotated source reports, a ``#`` or ``*`` caracter annotates all the
exempted lines, depending on whether 0 or at least 1 violation was exempted
over the whole section, respectively.

In synthetic text reports, a single indication is emitted for each exempted
region, and the indications for all the regions are grouped in a separate
report section, only present if there are exemption regions in the analysis
scope.

This *Exempted violations* section lists and counts the exempted regions,
displaying for each the source location span, the number of actually exempted
violations in the region, and the exemption justification text. For example:

::

  =========================
  == 3. EXEMPTED REGIONS ==
  =========================

  assert.adb:22:4-27:4: 2 exempted violations, justification:
  assertions are expected never to fail

  1 exempted region.

In addition, the regular Coverage Violations sections gets renamed as
"NON-EXEMPTED COVERAGE VIOLATIONS".
  

.. _consolidation:

Coverage Consolidation
======================

.. _osmetrics:

Object vs Source level metrics
==============================

Even tough the executable object code reflects semantics originally expressed
in the application sources, Object and Source level coverage metrics are of
very different nature, concerned with entities of very different kinds
(machine instructions vs high level constructs). This section's purpose is to
stress this point again and to illustrate with a couple of typical observable
differences in the |gcp| outputs.

