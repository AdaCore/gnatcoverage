.. _exemptions:

*******************
Coverage Exemptions
*******************

In some circumstances, there are good and well understood reasons why proper
coverage of some source construct is not achievable. The |gcp|
:dfn:`exemptions` facility was designed to allow abstracting these coverage
violations away from the genuine defects of a testing campaign.

Exemption Regions
=================

:dfn:`exemption regions` are lexical sections of sources in which
coverage violations are expected and can be justified.

For Ada with the |gnat| compilers, regions are defined by the insertion of
dedicated pragmas in the sources:

- ``pragma Annotate (Xcov, Exempt_On, "justification text");`` starts a
  region, providing some justification text that will be recalled in coverage
  reports.

- ``pragma Annotate (Xcov, Exempt_Off);`` closes the current exemption region.

There may be no overlap between exemption regions.

To illustrate, let us consider a common assertion control procedure in Ada,
which raises an exception when a provided condition, expected to be True,
happens to be False:

.. code-block:: ada

   procedure Eassert (T : Boolean) is
   begin
      pragma Annotate (Xcov, Exempt_On, "assert condition is never False");
      if not T then
         raise Program_Error;
      end if;
      pragma Annotate (Xcov, Exempt_Off);
   end Eassert;

We declare an exemption region to state that coverage violations are expected
and not to be considered as a testing campaign deficiency.  Indeed, we expect
never to reach here with ``T`` False in nominal circumstances, so the inner
``raise`` statement is never executed and the ``not T`` decision controlling
the ``if`` is only exercised one way.


Reporting about Coverage Exemptions
===================================

Exempted regions are reported as blocks in both the annotated source and the
synthetic text reports, for both source and object coverage metrics.

In annotated source reports, a ``#`` or ``*`` caracter annotates all the
exempted lines, depending on whether 0 or at least 1 violation was exempted
over the whole section, respectively.
For our ``Eassert`` example above, a typical :option:`=xcov` output
for :option:`stmt+decision` coverage for would be::

 0% of 2 lines covered
 Coverage level: stmt+decision
 ......
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
separate *Exempted Regions* report section, only present if there are
exemption regions in the analysis scope. This section lists the exempted
regions, displaying for each the source location span, the number of actually
exempted violations in the region, and the exemption justification text. It
also includes a total count of the number of exempted regions at the end.

The corresponding :option:`=report` excerpt below illustrates
this for the ``Eassert`` example::

   ...
   =========================
   == 3. EXEMPTED REGIONS ==
   =========================

   eassert.adb:8:4-12:4: 2 exempted violations, justification:
   assert condition never to be False

   1 exempted region.

   =========================
   == 4. ANALYSIS SUMMARY ==
   =========================

   No non-exempted STMT violation.
   No non-exempted DECISION violation.
   1 exempted region.

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

.. _exemption_alis:

Locating exemption annotations
==============================

While exemption regions are specified via annotations in source files,
exemptions are not criterion specific. They apply to both source and object
level criteria analyzed over the annotated regions.

In the example above, we would have used similar exemption annotations to deal
with expected object instruction and branch coverage failures in Eassert, as
the conditional branch used to implement the ``if`` statement is expected to
remain partially covered, as well as the sequence of machine instructions
triggerring the Ada exception raise.

As for Source Coverage Obligations for source level criteria, information
about the declared exemption regions is located in the :term:`Library
Information files` produced by the compiler for every compilation unit.

Similar mechanisms are available to designate units for which exemption
regions are of interest: the :option:`--alis` command line option, with
similar use rules as :ref:`--scos to designate source coverage obligations
<passing_scos>`, and the :ref:`high level project file support <passing_gpr>`
integrated in gnatcov.
