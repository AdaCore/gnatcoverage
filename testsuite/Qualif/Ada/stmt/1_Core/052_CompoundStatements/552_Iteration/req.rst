SC requirements for generalized Loop iterations (ARM 5.5.2)
===========================================================


%REQ_ID%

Generalized loop iterators are similar to simple For loop statements
in structure but provide direct access to components or flexible means
to run over iterator objects when the runtime profile allow it.

.. rubric:: Testing Strategy



We verify compliance with this requirement using

* A variety of loop constructs (for, while, reverse, unconditional),

* With and without exit statements for all variants, located at various places
  in the nested sequence (at the beginning, at the end, somewhere in between),

* Within a variety of source contexts (subprograms, package
  bodies, generic instances)

The following table summarizes the testcases:


.. qmlink:: TCIndexImporter

   *



