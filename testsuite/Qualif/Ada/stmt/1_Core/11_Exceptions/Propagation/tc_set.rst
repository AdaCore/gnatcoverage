Check SC with exceptions propagating across subprograms and packages
====================================================================

Check the effects of exceptions propagating across various levels of subrogram
calls. For a variety of situations, check results obtained in tests where

* Some exception gets raised,

* No exception gets raised, or

* Combinations thereof. 

The situation variations explore cases where the exception is raised
from

* a regular subprogram sequence of statements,
* a package elaboration sequence,
* within an exception handler,

check predefined or user-defined exceptions.

check stmt/decision/mcdc within subprograms called by handlers

check handlers within handlers

check exceptions which propagate through multiple levels of
subprogram calls.

check conditional re-raise

.. qmlink:: TCIndexImporter

   *
