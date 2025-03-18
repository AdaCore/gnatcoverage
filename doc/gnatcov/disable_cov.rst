.. _disable_cov:

*************************
Disable coverage analysis
*************************

It is possible to fully disable coverage analysis over a specific region. In
contrast to exemption regions, gnatcov skips over disabled coverage region and
treats them similarly as a no code region, e.g. with no source coverage
obligations.

Such a mechanism can typically be used to disable coverage for a code region
that may or may not expand according to the build configuration at use.

This mechanism only works when using source traces.

Defining Disabled Coverage Regions
==================================

:dfn:`Disabled Coverage Regions` are lexical sections of sources in which
coverage analysis is fully disabled.

For Ada with the |gnat| compilers, regions are defined by the insertion of
dedicated pragmas in the sources:

- ``pragma Annotate (Xcov, Cov_Off, "justification text");`` starts a region,
  providing some justification text that will be recalled in coverage reports.

- ``pragma Annotate (Xcov, Cov_On);`` closes the current disabled coverage
  region.

There may be no overlap between disabled coverage regions.

To illustrate, let us consider a common assertion control procedure in Ada:

.. code-block:: ada

   procedure Eassert (T : Boolean) is
   begin
      pragma Annotate (Xcov, Cov_Off, "my justification");
      if not T then
         raise Program_Error;
      end if;
      pragma Annotate (Xcov, Cov_On);
   end Eassert;

For C code, disabled coverage regions are defined using comment markers to
delimit the region

- Any comment containing the string ``GNATCOV_COV_OFF`` followed by a string in
  double quotes starts a region, the string within the double quotes being used
  as justification text that will be recalled in coverage reports.

- Any comment containing the string ``GNATCOV_COV_ON`` closes the current
  region.

The following assert function illustrates the definition of a disabled coverage
region:

.. code-block:: C

  void
  assert (bool x){
   // GNATCOV_COV_OFF "my justification"
    if (!x)
      abort();
   // GNATCOV_COV_ON
  }

It is also possible to define disabled coverage regions from external annotation
files, see :ref:`ext_annot` for more information.

Consolidating traces with varying disabled coverage regions
===========================================================

As stated, there can be a varying set of disabled coverage regions according
to the build configuration and when considering C/C++ preprocessing. If you
lie in that case, read thoroughly the information below.

It is not allowed to consolidate traces originating from instrumentations with
mismatching disabled coverage regions. If doing so, gnatcov will warn with
``traces for <unit_name> (from <filename>) are inconsistent with the corresponding Source Instrumentation Data``.

To circumvent this, it is mandatory to produce checkpoints using the
instrumentation artifacts the source trace originates from. GNATcov then allows
consolidation of checkpoints with mismatching disabled coverage regions. See
:ref:`consolidation` for more information.


Reporting about disabled coverage regions
=========================================

Exempted regions are reported as blocks in both the annotated source and the
synthetic text reports, for both source and object coverage metrics.  In
annotated source reports, a ``D`` annotates all the lines of a disabled coverage
region.

For our ``Eassert`` above, a typical :cmd-option:`=xcov` output for
:cmd-option:`stmt+decision` coverage for would be::

   6 .: procedure Eassert (T : Boolean) is
   7 .: begin
   8 D:    pragma Annotate (Xcov, Cov_Off, "my justification");
   9 D:    if not T then
  10 D:       raise Program_Error;
  11 D:    end if;
  12 D:    pragma Annotate (Xcov, Cov_On);
  13 .: end Eassert;


In synthetic text reports, a single indication is emitted for each disabled
coverage region as a whole, and the indications for all the regions are grouped
in a separate *Disabled Coverage Regions* report section, only present if there
are disabled coverage regions in the analysis scope.

The corresponding :cmd-option:`=report` excerpt below illustrates this for the
``Eassert`` example::

   ...
   ==================================
   == 3. DISABLED COVERAGE REGIONS ==
   ==================================

   eassert.adb:8:4-12:4: justification:
   "my justification"

   1 region with disabled coverage.

   =========================
   == 4. ANALYSIS SUMMARY ==
   =========================

   1 region with disabled coverage.
