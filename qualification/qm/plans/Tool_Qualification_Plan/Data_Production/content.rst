Production of Qualification Data
================================

For a given project, the production of qualification data proceeds in several
steps described hereafter:

Settle on the |opcond_section_title|
************************************

As the Tool Developer, we first discuss with the Tool User to define the use
that will be made of the tool, agreeing on items such as the tool interface,
the source language version, the runtime profile and language features, the
software level.
This eventually translates as specific indications in the
|opcond_section_title_ref| section of the |tor_doc| document and in the
:ref:`qualified-interface` section of the |plans_doc| document, then |tors|
and their Tests derivatives are refined to accommodate changes in the
scope for which the tool had been qualified so far.

Produce TORs, Test Cases and Tests Artifacts
********************************************

The current set of TORs is organized very hierarchically along the following
major lines:

* The coverage criteria associated with software levels,

* The constructs allowed by the source language, influenced by the established
  conditions of use, and

* General coverage analysis facilities, such as the abiliy to perform coverage
  consolidation.

The general organization is described in the
|torintro_section_title_ref| of the |tor_doc| document.

A TOR always comes with a Testing Strategy, to be implemented by Test Cases
and eventually Tests. The TOR expresses expected behavior of the tool and the
testing strategy introduces the set of Test Cases. It describes the general
ideas governing the choice of this specific set when needed, and explains how
the set of Tests is constructed for each Test Case when common guidelines were
used.

Each Test Case addresses a part of what its parent TOR incurs, e.g.  on a
particular instance of a language construct. For example, we have a general
core requirement regarding MCDC assessments and derive families of Test Cases
to validate the tool behavior on variations over forms and sizes of decisions,
over the context in which decisions may occur, over the possible forms of
expression operands.

Then for a given Test Case (say, an *A and then B* Ada expression controlling
an *if* statement, with *A* and *B* both simple Boolean variables), several
tests verify the tool behavior across a set of execution scenarii. Each test
checks results for a specific sequence of calls into the code to analyse,
providing particular values for variables involved in a decision for
example. A single such valuation or several of them are performed with a
specific coverage objective purpose, for instance evaluating a decision both
True and False but demonstrating independent influence of only the first
condition.

Section |testproc_section_title_ref| of the |tor_doc| document
provides more details on how Tests are composed, and in particular on how
expected results are stated.

Completeness of the TORs with respect to language features of relevance for
the project is tracked through an LRM-to-TORs-and-Test-Cases traceability
matrix provided as part of the |tor_doc| document.

Additional TORs are introduced or existing TORs are adjusted on a case by case
basis when the scope of possible uses of the tool widens somehow, most
commonly when the set of source language constructs for which the tool needs
to be qualified expands.

The adjustment of an existing TOR might incur a change in the TOR expression
itself and/or a refinement of it's testing strategy to encompass what the
new scope requires.


Execute Tests to Produce Test-Results Artifacts
***********************************************

This is achieved in the Qualification Environment by launching a script which
automatically selects the relevant set of tests for the target software level
and runs them with provided control parameters. The tests are taken from a
branch dedicated to the kit construction in AdaCore's repositories.

This produces execution logs and data files that are later on gathered to
elaborate the |str_doc| report. The Qualification Team verifies at this point
that test results conform to expectations, making sure that the only failures,
if any, are understood and registered in AdaCore's tracking system.

Produce the Qualification Kit
*****************************

This is also achieved automatically by a collection of tools coordinated by a
toplevel script provided with the name of a branch in AdaCore's repositories
where the kit is maintained and the (possibly remote) location of the
directory where the testsuite execution took place.

The script collects the artifacts, builds the PDF and HTML versions of the
documents and bundles everything together to constitute the kit per se. It
also performs consistency and tracebility checks, producing a log of what
those checks find out. The Qualification Team reviews this log and verifies
the general consistency of the documents before handing them to the Quality
Assurance team.
