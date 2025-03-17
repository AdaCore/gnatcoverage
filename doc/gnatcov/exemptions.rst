.. _exemptions:

*******************
Coverage Exemptions
*******************

In some circumstances, there are good and well understood reasons why proper
coverage of some source construct is not achievable. The |gcp|
:dfn:`exemptions` facility was designed to allow abstracting these coverage
violations away from the genuine defects of a testing campaign.

Defining :term:`Exemption Regions <Exemption Region>`
=====================================================

:dfn:`Exemption regions` are lexical sections of sources in which coverage
violations are expected and can be justified.

For Ada with the |gnat| compilers, regions are defined by the insertion of
dedicated pragmas in the sources:

- ``pragma Annotate (Xcov, Exempt_On, "justification text");`` starts a
  region, providing some justification text that will be recalled in coverage
  reports.

- ``pragma Annotate (Xcov, Exempt_Off);`` closes the current exemption region.

There may be no overlap between exemption regions.

To illustrate, let us consider a common assertion control procedure in Ada:

.. code-block:: ada

   procedure Eassert (T : Boolean) is
   begin
      pragma Annotate (Xcov, Exempt_On, "assert condition never to be False");
      if not T then
         raise Program_Error;
      end if;
      pragma Annotate (Xcov, Exempt_Off);
   end Eassert;

We expect never to reach here with ``T`` False, so we declare an exemption
region to state that coverage violations across this region are expected and
not a testing campaign deficiency.

For C code, exemptions are only supported with
:term:`source traces<Source Trace>` and are defined using comment markers to
delimit the exempted regions:

- Any comment containing the string ``GNATCOV_EXEMPT_ON`` followed by a string
  in double quotes starts a region, the string within the double quotes being
  used as justification text that will be recalled in coverage reports.

- Any comment containing the string ``GNATCOV_EXEMPT_OFF`` closes the current
  exemption region.

The following assert function illustrates the definition of an exemption
block:

.. code-block:: C

  void
  assert (bool x){
   // GNATCOV_EXEMPT_ON "assert condition never to be False"
    if (!x)
      abort();
   // GNATCOV_EXEMPT_OFF
  }

As in the first example, we never expect to reach this function with x false,
so an exemption region is declared to state that all coverage violations
within the region are expected.

An exemption comment marker may not intersect any coverage obligation,
such as a statement or a decision, as in the following example attempting
to exempt a decision only partially:

.. code-block:: C

  if(a && /*GNATCOV_EXEMPT_ON "justification"*/ b /*GNATCOV_EXEMPT_OFF*/){
    ...
  }

Such markers are ignored by |gcvins|, after emitting a warning.

It is also possible to define exemption regions trough external annotation
files, see :ref:`ext_annot` for more information.

Reporting about coverage exemptions
===================================

Exempted regions are reported as blocks in both the annotated source and the
synthetic text reports, for both source and object coverage metrics.  In
annotated source reports, a ``#`` or ``*`` character annotates all the exempted
lines, depending on whether 0 or at least 1 violation was exempted over the
whole section, respectively.  For our ``Eassert`` example above, a typical
:cmd-option:`=xcov` output for :cmd-option:`stmt+decision` coverage for would
be::

   6 .: procedure Eassert (T : Boolean) is
   7 .: begin
   8 *:    pragma Annotate (Xcov, Exempt_On, "assert condition never to be False");
   9 *:    if not T then
  10 *:       raise Program_Error;
  11 *:    end if;
  12 *:    pragma Annotate (Xcov, Exempt_Off);
  13 .: end Eassert;

The whole block is marked with ``*`` annotations to indicate that some
violations were actually exempted; 2 in this case: the statement coverage
violation for the ``raise`` and the decision coverage violation for the ``if``
control.

In synthetic text reports, a single indication is emitted for each exempted
region as a whole, and the indications for all the regions are grouped in a
separate *Exempted Regions* report section, only present if there are exemption
regions in the analysis scope. This section lists the exempted regions,
displaying for each the source location span, the number of actually exempted
violations in the region, the exemption justification text and the observed
exempted violations per regions. It also includes a total count of the number
of exempted regions and another for exempted violations across all sections at
the end.

The corresponding :cmd-option:`=report` excerpt below illustrates this for the
``Eassert`` example::

   ...
   =========================
   == 3. EXEMPTED REGIONS ==
   =========================

   eassert.adb:8:4-12:4: 2 exempted violations, justification:
   "assert condition never to be False"

   Exempted violations:
   eassert.adb:8:4: decision outcome TRUE never exercised
   eassert.adb:9:8: statement not executed

   1 exempted region, 2 exempted violations.

   =========================
   == 4. ANALYSIS SUMMARY ==
   =========================

   No non-exempted STMT violation.
   No non-exempted DECISION violation.
   1 exempted region, 2 exempted violations.

The *Coverage Violations* section is renamed to convey that it contains
"NON-EXEMPTED" violations only, and the *Analysis Summary* counters are
adjusted in a similar manner. The number of exempted regions is added to
the list of counters in this section.

If the executed tests actually trigger an assertion failure, there is no
coverage violation to be exempted any more and this translates as visible
differences in the reports:

In annotated sources, the region is annotated with ``#`` signs instead of
``*``, as in::

   6 .: procedure Eassert (T : Boolean) is
   7 .: begin
   8 #:    pragma Annotate (Xcov, Exempt_On, "assert condition never to be False");
   9 #:    if not T then
  10 #:       raise Program_Error;
  11 #:    end if;
  12 #:    pragma Annotate (Xcov, Exempt_Off);
  13 .: end Eassert;

In synthetic reports, the count of exempted violations is 0, like::

  =========================
  == 3. EXEMPTED REGIONS ==
  =========================

  eassert.adb:8:4-12:4: 0 exempted violation, justification:
  assert condition never to be False

  1 exempted region.

Undetermined Coverage state and Exemptions
------------------------------------------

For each exemption region in which there are obligations with undetermined
coverage state, the synthetic text report will indicate the count of
undetermined coverage obligations in the region, in addition to the number
of violations.

In the annotated sources, exemption regions with *only* undetermined coverage
items are annotated with ``@`` signs instead of ``*``. If there are both
undetermined coverage obligations as well as violations in the exemption
region, the corresponding lines is still annotated with ``#``.

.. _ocov_exemptions:

Object coverage exemptions
==========================

Exemption regions specified via annotations in source files actually apply
to both source and object level criteria analyzed over the annotated regions.

In the previous example, we would have used similar exemption annotations to
deal with expected object instruction and branch coverage failures in Eassert,
as the conditional branch used to implement the ``if`` statement is expected
to remain partially covered, as well as the sequence of machine instructions
triggerring the Ada exception raise.

As for Source Coverage Obligations for source level criteria, information about
the declared exemption regions is located in the :term:`Library Information
files <Library Information file>` produced by the compiler for every
compilation unit. The mechanisms described in chapter :ref:`sunits` can then
also be used to designate units for which exemptions regions should be
accounted for.
