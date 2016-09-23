SC requirements for parameter modes
===================================


%REQ_ID%

The formal mode of subprogram parameters shall not influence the coverage
assessment of statements referring to those parameters.


.. rubric:: Testing Strategy

We verify the correctness of statement coverage assessments over subprograms
accepting formal parameters with a variety of modes, covering all the modes
allowed by the language reference manual (in, out, in-out, on procedures or
functions).

.. qmlink:: TCIndexImporter

   *

