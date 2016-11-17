SC requirements for Loop statements (ARM 5.5)
=============================================


%REQ_ID%



A LOOP statement contains a loop *header* (optional iteration scheme followed
by the `loop` keyword) and a loop body that may optionally include one or more
EXIT statements.

In addition to the common requirements that apply to the nested statements,
FOR or WHILE loop headers that are never reached shall be reported uncovered.


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



