Operational Environment
=======================

The correctness of source coverage assessments performed by the tool depends
on a few external rules that shall be obeyed. First comes a set of general
rules that always apply:

.. csv-table::
   :header: "Rule #", ""
   :widths: 20, 65
   :delim:  |

   1 (Compiler version) | "The tool may only be used with a GNAT/GCC compiler
   version identified as suitable by the tool provider."
   2 (Base compilation flags) | "All the applicative code shall be compiled
   with the ``-g -fpreserve-control-flow`` command-line options, together with
   ``-gnateS`` for Ada sources."
   3 (Optimization flags) | "Up to GNAT Pro 6.4.2, -O0 is the only supported
   level. Later releases will support -O1, with or without inlining. -O2 or
   individual optimization flags are not supported. For Ada, suppression of
   run-time checks with ``-gnatp`` is allowed, however not mandatory."
   4 (Coding standard) | "For criteria involving decisions or conditions in
   the DO-178B sense, binary Boolean operators shall be restricted to those
   with short-circuit semantics. These are ``and then`` and ``or else`` in
   Ada, with the rule enforced by the ``No_Direct_Boolean_Operator``
   Restriction pragma in the GNAT Pro series of compilers."


Extra options are allowed when they are known not to influence code
generation, as, for example, warning control options.

In any case, the tool behavior correctness for a particular combination of
versions and command-line options shall be verified by a complete testsuite
run configured for the target qualification level, producing a *Software Test
Results* report clear of any test failure.

