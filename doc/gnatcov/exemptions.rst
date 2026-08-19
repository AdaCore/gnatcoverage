.. _exemptions:

*******************
Coverage Exemptions
*******************

In some circumstances, there are good and well understood reasons why proper
coverage of some source construct is not achievable. The |gcp|
:dfn:`exemptions` facility was designed to allow abstracting these coverage
violations away from the genuine defects of a testing campaign.

.. _exemption_region:

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

- Comments matching ``GNATCOV_EXEMPT_ON`` or ``GNATCOV_EXEMPT_ON("my
  justification")`` start a region, the string within the double quotes being
  used as justification text that will be recalled in coverage reports.

- Comments matching ``GNATCOV_EXEMPT_OFF`` close the current region.

The following assert function illustrates the definition of an exemption
block:

.. code-block:: C

  void
  assert (bool x){
   // GNATCOV_EXEMPT_ON("assert condition never to be False")
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

  if(a && /*GNATCOV_EXEMPT_ON("justification")*/ b /*GNATCOV_EXEMPT_OFF*/){
    ...
  }

Such markers are ignored by |gcvins|, after emitting a warning.

It is also possible to define exemption regions trough external annotation
files, see :ref:`ext_annot` for more information.

Defining :term:`Fine Grained Exemptions <Fine Grained Exemption>`
=================================================================

:dfn:`Fine Grained Exemptions` are annotations to state that violations of
precise source coverage obligations are expected, with a justification message.
These annotations are similar to the ones that define exemption regions:

.. code-block:: ada

   --  In Ada

   pragma Annotate
     (Xcov,
      Exemption_Kind,  -- Identifier to designate what to exempt
      --  Potential exemption kind-dependent details go here
      "Message"  -- Justification for the exemption);

.. code-block:: c

   // In C

   // GNATCOV_EXEMPTION_KIND(/* ... details ... */, "Message")

These annotations must appear right before the statement/declaration in which
to find the source coverage obligation to exempt:

.. code-block:: Ada

   --  Exemption_1 targets a construct in Statement_Or_Declaration_A

   pragma Annotate (Xcov, Exemption_1, "Justification");

   Statement_Or_Declaration_A;

   --  Exemption_2 .. 4 target a construct in Statement_Or_Declaration_B;
   --  Statement_Or_Declaration_C is out of reach.

   pragma Annotate (Xcov, Exemption_2, "Justification");
   pragma Annotate (Xcov, Exemption_3, "Justification");
   pragma Annotate (Xcov, Exemption_4, "Justification");

   Statement_Or_Declaration_B;
   Statement_Or_Declaration_C;

   --  Exemption_5 targets a construct in Statement_Or_Declaration_D

   pragma Annotate (Xcov, Exemption_5, "Justification");

   Statement_Or_Declaration_D;

.. _fine_grained_exemption_decision_outcome:

Decision Outcome Exemptions
---------------------------

For decision coverage, each of the two decision outcomes can be exempted
separately. In Ada, the following will exempt the outcome True of the ``Debug``
decision:

.. code-block:: ada

  pragma Annotate (Xcov, Exempt_Decision_Outcome, True, "Debug code");
  Put_Line ("Content" & (if Debug then " (debug)" else ""));

The equivalent construct in C/C++ would be:

.. code-block:: c

  // GNATCOV_EXEMPT_DECISION_OUTCOME(true, "Debug code")
  printf ("Content%s", debug ? " (debug)" : "");

.. _exemption_decision_offset:

When there are multiple decisions in the same statement, it is possible to
specify a "decision offset" ``N``, instructing to skip ``N`` decisions after
reaching the one to exempt. For instance:

.. code-block:: ada

  pragma Annotate (Xcov, Exempt_Decision_Outcome, True, 2, "Debug code");
  Procedure_Call
    (Arg_1 => (if Flag_1 then 'A' else 'B'),
     Arg_2 => (if Flag_2 then 'C' elsif Flag_3 then 'D' else 'E'));

There are 3 decisions here: in source order: ``Flag_1``, ``Flag_2`` and
``Flag_3``. Without a decision offset, the exemption would target the first
decision (``Flag_1``), but with the decision offset 2, the exemption targets
``Flag_3``: 2 decisions are skipped (``Flag_1`` and ``Flag_2``).

.. _fine_grained_exemption_decision_condition:

Decision Condition Exemptions
-----------------------------

For MCDC, the need to demonstrate the independent influence of each condition
on the decision outcome can be exempted.

.. code-block:: ada

   pragma Annotate (Xcov, Exempt_Decision_Condition, 2, "Debug code");
   if Message_Requested or else Debug then
      Send_Message;
   end if;

In this example, 2 is the index of the condition to exempt. For a given
decision, indexes are assigned from left to right starting at 1:
``Message_Requested`` is the condition at index 1, ``Debug`` is the condition
at index 2.

With this fine grained exemption, it is no longer necessary to demonstrate the
independent influence of the ``Debug`` condition, so the following evaluation
vectors for the decision will be enough to get no violation in the coverage
report:

* ``False or else False`` (outcome: ``False``)
* ``True or else XXX`` (outcome: ``True``)

whereas without the exemption, an additional ``False or else True`` would be
needed to reach full MCDC coverage.

Below is the equivalent example in C/C++:

.. code-block:: c

   // GNATCOV_EXEMPT_DECISION_CONDITION(2, "Debug code")
   if (message_requested || debug)
     send_message ();

Note that for ATCC, the exemption covers the mere evaluation of the designated
condition as part of a decision evaluation that reaches its outcome True.

As with :ref:`decision outcome exemptions <exemption_decision_offset>`, it is
possible to specify a decision offset for cases when there are multiple
decisions in the statement:

.. code-block:: ada

   pragma Annotate (Xcov, Exempt_Decision_Condition, 3, 1, "Debug code");
   Procedure_Call
     (Arg_1 => (if Flag_1 then 'A' else 'B'),
      Arg_2 => (if Flag_2
                   and then Flag_3
                   and then Flag_4
                then 'C'
                else 'D'),
      Arg_3 => (if Flag_5 then 'E' else 'F'));

Here, the statement that follows the exemption annotation has 3 decisions. The
decision at offset 1 (i.e. ``Flag_2 and then Flag_3 and then Flag_4``) has 3
conditions, so it is ``Flag_4`` that is exempted.

.. _fine_grained_exemption_full_decision:

Full Decision Exemptions
------------------------

|gcv| supports the following convenience annotation to exempt all relevant
outcomes (i.e. False and True for decision coverage and MCDC, only True for
assertion coverage) and conditions for a given decision:

.. code-block:: ada

   --  Exempt all decisions and conditions for the next decision
   pragma Annotate (Xcov, Exempt_Full_Decision, "Justification");

   --  Exempt all decisions and conditions for the decision
   --  at offset 2.
   pragma Annotate (Xcov, Exempt_Full_Decision, 2, "Justification");

.. code-block:: c

   // GNATCOV_EXEMPT_FULL_DECISION("Justification")
   // GNATCOV_EXEMPT_FULL_DECISION(2, "Justification")

In both examples above, ``2`` is the optional :ref:`decision offset
<exemption_decision_offset>`.

.. _fine_grained_exemption_branch:

Branch Exemptions
-----------------

The branch exemption annotation is a convenience helper that can be put first
in an ``if``/``elsif``/``else`` block (Ada and C/C++): it is expanded into:

* a decision outcome exemption for the relevant outcome of the closest
  controlling decision;
* an exemption region for the statements that contain the branch exemption.

For example:

.. code-block:: ada

   if Debug then
      pragma Annotate (Xcov, Exempt_Branch, "Debug code");
      Put_Line ("Execution went here");
   elsif Profiling then
      Increment_Counter;
   end if;

Here, the outcome True of the ``Debug`` decision is exempted, as well as the
call to ``Put_Line``. However, if the annotation was put before the call to
``Increment_Counter``, then the outcome True of ``Profiling`` and the call to
``Increment_Counter`` would be exempted.

In C/C++, the equivalent example would be:

.. code-block:: c

   if (debug)
     // GNATCOV_EXEMPT_BRANCH("Debug code")
     puts ("Execution went here");
   else if (profiling)
     increment_counter ();

Specifically in Ada, it is also possible to set a branch exemption in a ``when
...  =>`` clause: in that case, no decision outcome is exempted, but all
statements in the branch are exempted:

.. code-block:: ada

   case State is
      when Uninitialized =>
         Initialize;

      when Initialized =>
         Make_Progress;

      when Error =>
         pragma Annotate (Xcov, Exempt_Branch, "defensive code");
         Log_Unreachable ("State = Error");
         raise Program_Error;
   end case;

.. _fine_grained_exemption_manual_decision_evaluation:

Manual Decision Evaluations
---------------------------

While they are not exactly exemptions, decision evaluation vectors are conveyed
to |gcv| through the annotation mechanism: coverage report production will then
act as if the decision was evaluated with the given condition values.

.. code-block:: ada

   pragma Annotate
     (Xcov, Manual_Decision_Evaluation, False, False, "Tested manually");
   pragma Annotate
     (Xcov, Manual_Decision_Evaluation, True, False, "Tested manually");
   if A and then B then
      Do_Something;
   end if;

In this example, |gcv| will consider that the ``A and then B`` decision was
evaluated as ``False and then XXX`` and ``True and then False``, so MCDC will
be fully achieved for this decision as soon as it is evaluated at run time with
``True, True``.

Note that the in the ``False, False`` evaluation vector, the second condition
valuation is not meaningful since, because of the short-circuiting behavior of
``and then``, ``B`` is not evaluated when ``A`` evaluates to False: in that
case, the second boolean value passed to ``Manual_Decision_Evaluation`` is just
disregarded, yet its presence is necessary in order to have exactly one
valuation per condition.

Below is the equivalent example in C/C++:

.. code-block:: c

   // GNATCOV_MANUAL_DECISION_EVALUATION(false, false, "Tested manually")
   // GNATCOV_MANUAL_DECISION_EVALUATION(true, false, "Tested manually")
   if (a && b)
      do_something ();

As with :ref:`decision outcome exemptions <exemption_decision_offset>`, it is
possible to specify a decision offset for cases when there are multiple
decisions in the statement:

.. code-block:: ada

   pragma Annotate
     (Xcov, Manual_Decision_Evaluation, False, False, 1, "Debug code");
   Procedure_Call
     (Arg_1 => (if Flag_1 then 'A' else 'B'),
      Arg_2 => (if Flag_2 and then Flag_3 then 'C' else 'D'),
      Arg_3 => (if Flag_5 then 'E' else 'F'));

Reporting about coverage exemptions
===================================

Exemption regions
-----------------

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

Fine grained exemptions
-----------------------

In the annotated source, fine grained exemptions are reported in the line range
that covers the relevant source coverage obligation, with the same signs as
exemption regions (``#`` and ``*``) depending on whether the exempted
obligation had a violation. Note that unexempted violations are reported in
priority over exempted violations::

   --  The whole statement was not executed

   8 .: pragma Annotate (Xcov, Exempt_Decision_Outcome, True, "Debug code");
   9 -: Put_Line ("Content" & (if Debug then " (debug)" else ""));

   --  The statement was executed, Debug was evaluated to False

   8 .: pragma Annotate (Xcov, Exempt_Decision_Outcome, True, "Debug code");
   9 *: Put_Line ("Content" & (if Debug then " (debug)" else ""));

   --  The statement was executed and Debug evaluated to both False and True

   8 .: pragma Annotate (Xcov, Exempt_Decision_Outcome, True, "Debug code");
   9 #: Put_Line ("Content" & (if Debug then " (debug)" else ""));

In synthetic text reports, fine grained exemptions that were triggered (i.e.
that prevented the emission of a note in the Coverage Violations section) are
listed in the Fine Grained Exempted Violatinos section::

   =========================================
   == 3. FINE GRAINED EXEMPTED VIOLATIONS ==
   =========================================

   pkg.adb:8:10: decision outcome TRUE never exercised (exempted: Debug code)

   1 fine grained exempted item.

Manual decision evaluations are visible in annotated source reports, but only
when including details (``xcov+``, ``html``)::

    8 .: pragma Annotate
    9 .:  (Xcov, Manual_Decision_Evaluation, False, False, "Tested manually");
   10 .: pragma Annotate
   11 .:  (Xcov, Manual_Decision_Evaluation, True, False, "Tested manually");
   12 +: if A and then B then
   pkg.adb:12:15: including manual decision evaluation:
     F - -> FALSE (justification: Tested manually)

In synthetic text reports, they are also included in the Fine Grained Exempted
Violations section::

   =========================================
   == 3. FINE GRAINED EXEMPTED VIOLATIONS ==
   =========================================

   pkg.adb:12:15: including manual decision evaluation:
     F - -> FALSE (justification: Tested manually)

   1 fine grained exempted item.

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
triggering the Ada exception raise.

As for Source Coverage Obligations for source level criteria, information about
the declared exemption regions is located in the :term:`Library Information
files <Library Information file>` produced by the compiler for every
compilation unit. The mechanisms described in chapter :ref:`sunits` can then
also be used to designate units for which exemptions regions should be
accounted for.
